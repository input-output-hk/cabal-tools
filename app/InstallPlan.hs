{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State as State
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Client.Compat.Prelude
import Distribution.Client.Config
import Distribution.Client.Dependency
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils qualified as IndexUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy (ProjectConfigSkeleton, instantiateProjectConfigSkeleton)
import Distribution.Client.ProjectPlanOutput
import Distribution.Client.ProjectPlanning.Types as Ty
import Distribution.Client.RebuildMonad (FileMonitor, Rebuild, newFileMonitor, rerunIfChanged, runRebuild)
import Distribution.Client.Store
import Distribution.Client.Types
import Distribution.PackageDescription qualified as PD
import Distribution.Simple.Compiler (Compiler (compilerId), PackageDB (GlobalPackageDB), compilerInfo)
import Distribution.Simple.Program
import Distribution.Simple.Program.Db (configuredPrograms)
import Distribution.Simple.Program.Find
import Distribution.Simple.Setup (flagToList)
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.SourcePackage (SourcePackage)
import Distribution.System (Platform (Platform))
import Distribution.Utils.LogProgress
import Distribution.Verbosity (moreVerbose)
import Distribution.Verbosity qualified as Verbosity
import My.Client.ProjectPlanning (applyPackageDbFlags, configureCompiler, elaborateInstallPlan, getInstalledPackages, getPackageSourceHashes, getPkgConfigDb, getSourcePackages, improveInstallPlanWithInstalledPackages, instantiateInstallPlan, packageLocationsSignature, planPackages, programDbSignature, reportPlanningFailure, shouldBeLocal, userInstallDirTemplates)
import Options.Applicative
import Prelude ()

main :: IO ()
main =
  join $
    execParser $
      Options.Applicative.info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Extracts a cabal install plan")
  where
    optionsParser = do
      verbosity <-
        option
          (eitherReader eitherParsec)
          ( long "verbosity"
              <> metavar "VERBOSITY"
              <> value Verbosity.normal
              <> help "Verbosity"
          )
      inputDir <- optional (argument str (metavar "INPUT-DIR"))
      outputDir <- argument str (metavar "OUTPUT-DIR" <> value "./out")
      pure $ doMain verbosity inputDir outputDir

doMain :: Verbosity -> Maybe FilePath -> [Char] -> IO ()
doMain verbosity inputDir outputDir = do
  cabalDir <- getCabalDir
  let cabalDirLayout = defaultCabalDirLayout cabalDir

  Right projectRoot <- findProjectRoot inputDir Nothing
  let distDirLayout = defaultDistDirLayout projectRoot (Just outputDir)

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      -- more verbose here to list the project files which have affected
      -- the project configuration with no extra options
      (moreVerbose verbosity)
      httpTransport
      distDirLayout
      mempty

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  putStrLn $ "Writing detailed plan to " ++ outputDir

  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

rebuildProjectConfig ::
  Verbosity ->
  HttpTransport ->
  DistDirLayout ->
  ProjectConfig ->
  IO
    ( ProjectConfig,
      [PackageSpecifier UnresolvedSourcePackage]
    )
rebuildProjectConfig
  verbosity
  httpTransport
  distDirLayout@DistDirLayout
    { distProjectRootDirectory,
      distDirectory,
      distProjectCacheFile,
      distProjectCacheDirectory,
      distProjectFile
    }
  cliConfig = do
    fileMonitorProjectConfigKey <- do
      configPath <- getConfigFilePath projectConfigConfigFile
      return (configPath, distProjectFile "")

    (projectConfig, localPackages) <-
      runRebuild distProjectRootDirectory
        $ rerunIfChanged
          verbosity
          fileMonitorProjectConfig
          fileMonitorProjectConfigKey -- todo check deps too?
        $ do
          liftIO $ Cabal.info verbosity "Project settings changed, reconfiguring..."
          liftIO $ Cabal.createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory
          projectConfigSkeleton <- phaseReadProjectConfig
          -- have to create the cache directory before configuring the compiler
          (compiler, Platform arch os, _) <- configureCompiler verbosity distDirLayout (fst (PD.ignoreConditions projectConfigSkeleton) <> cliConfig)
          let projectConfig = instantiateProjectConfigSkeleton os arch (compilerInfo compiler) mempty projectConfigSkeleton
          localPackages <- phaseReadLocalPackages (projectConfig <> cliConfig)
          return (projectConfig, localPackages)

    Cabal.info verbosity $
      unlines $
        ("this build was affected by the following (project) config files:" :) $
          [ "- " ++ path
            | Explicit path <- Set.toList $ projectConfigProvenance projectConfig
          ]

    return (projectConfig <> cliConfig, localPackages)
    where
      ProjectConfigShared {projectConfigConfigFile} =
        projectConfigShared cliConfig

      ProjectConfigShared {projectConfigIgnoreProject} =
        projectConfigShared cliConfig

      fileMonitorProjectConfig ::
        FileMonitor
          (FilePath, FilePath)
          (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage])
      fileMonitorProjectConfig =
        newFileMonitor (distProjectCacheFile "config")

      -- Read the cabal.project (or implicit config) and combine it with
      -- arguments from the command line
      --
      phaseReadProjectConfig :: Rebuild ProjectConfigSkeleton
      phaseReadProjectConfig = do
        readProjectConfig verbosity httpTransport projectConfigIgnoreProject projectConfigConfigFile distDirLayout

      -- Look for all the cabal packages in the project
      -- some of which may be local src dirs, tarballs etc
      --
      phaseReadLocalPackages ::
        ProjectConfig ->
        Rebuild [PackageSpecifier UnresolvedSourcePackage]
      phaseReadLocalPackages
        projectConfig@ProjectConfig
          { projectConfigShared,
            projectConfigBuildOnly
          } = do
          pkgLocations <- findProjectPackages distDirLayout projectConfig
          -- Create folder only if findProjectPackages did not throw a
          -- BadPackageLocations exception.
          liftIO $ do
            Cabal.createDirectoryIfMissingVerbose verbosity True distDirectory
            Cabal.createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

          fetchAndReadSourcePackages
            verbosity
            distDirLayout
            projectConfigShared
            projectConfigBuildOnly
            pkgLocations

rebuildInstallPlan ::
  Verbosity ->
  DistDirLayout ->
  CabalDirLayout ->
  ProjectConfig ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  -- | @(improvedPlan, elaboratedPlan, _, _, _)@
  IO
    ( ElaboratedInstallPlan, -- with store packages
      ElaboratedInstallPlan, -- with source packages
      ElaboratedSharedConfig,
      IndexUtils.TotalIndexState,
      IndexUtils.ActiveRepos
    )
rebuildInstallPlan
  verbosity
  distDirLayout@DistDirLayout
    { distProjectRootDirectory,
      distProjectCacheFile
    }
  CabalDirLayout
    { cabalStoreDirLayout
    } = \projectConfig localPackages ->
    runRebuild distProjectRootDirectory $ do
      progsearchpath <- liftIO getSystemSearchPath
      let projectConfigMonitored = projectConfig {projectConfigBuildOnly = mempty}

      -- The overall improved plan is cached
      rerunIfChanged
        verbosity
        fileMonitorImprovedPlan
        -- react to changes in the project config,
        -- the package .cabal files and the path
        (projectConfigMonitored, localPackages, progsearchpath)
        $ do
          -- And so is the elaborated plan that the improved plan based on
          (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos) <-
            rerunIfChanged
              verbosity
              fileMonitorElaboratedPlan
              ( projectConfigMonitored,
                localPackages,
                progsearchpath
              )
              $ do
                compilerEtc <- phaseConfigureCompiler projectConfig
                _ <- phaseConfigurePrograms projectConfig compilerEtc
                (solverPlan, pkgConfigDB, totalIndexState, activeRepos) <-
                  phaseRunSolver
                    projectConfig
                    compilerEtc
                    localPackages
                ( elaboratedPlan,
                  elaboratedShared
                  ) <-
                  phaseElaboratePlan
                    projectConfig
                    compilerEtc
                    pkgConfigDB
                    solverPlan
                    localPackages

                phaseMaintainPlanOutputs elaboratedPlan elaboratedShared
                return (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)

          -- The improved plan changes each time we install something, whereas
          -- the underlying elaborated plan only changes when input config
          -- changes, so it's worth caching them separately.
          improvedPlan <- phaseImprovePlan elaboratedPlan elaboratedShared

          return (improvedPlan, elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)
    where
      fileMonitorSolverPlan = newFileMonitorInCacheDir "solver-plan"
      fileMonitorSourceHashes = newFileMonitorInCacheDir "source-hashes"
      fileMonitorElaboratedPlan = newFileMonitorInCacheDir "elaborated-plan"
      fileMonitorImprovedPlan = newFileMonitorInCacheDir "improved-plan"

      newFileMonitorInCacheDir :: Eq a => FilePath -> FileMonitor a b
      newFileMonitorInCacheDir = newFileMonitor . distProjectCacheFile

      -- Configure the compiler we're using.
      --
      -- This is moderately expensive and doesn't change that often so we cache
      -- it independently.
      --
      phaseConfigureCompiler ::
        ProjectConfig ->
        Rebuild (Compiler, Platform, ProgramDb)
      phaseConfigureCompiler = configureCompiler verbosity distDirLayout

      -- Configuring other programs.
      --
      -- Having configred the compiler, now we configure all the remaining
      -- programs. This is to check we can find them, and to monitor them for
      -- changes.
      --
      -- TODO: [required eventually] we don't actually do this yet.
      --
      -- We rely on the fact that the previous phase added the program config for
      -- all local packages, but that all the programs configured so far are the
      -- compiler program or related util programs.
      --
      phaseConfigurePrograms ::
        ProjectConfig ->
        (Compiler, Platform, ProgramDb) ->
        Rebuild ()
      phaseConfigurePrograms projectConfig (_, _, compilerprogdb) = do
        -- Users are allowed to specify program locations independently for
        -- each package (e.g. to use a particular version of a pre-processor
        -- for some packages). However they cannot do this for the compiler
        -- itself as that's just not going to work. So we check for this.
        liftIO $
          checkBadPerPackageCompilerPaths
            (configuredPrograms compilerprogdb)
            (getMapMappend (projectConfigSpecificPackage projectConfig))

      -- TODO: [required eventually] find/configure other programs that the
      -- user specifies.

      -- TODO: [required eventually] find/configure all build-tools
      -- but note that some of them may be built as part of the plan.

      -- Run the solver to get the initial install plan.
      -- This is expensive so we cache it independently.
      --
      phaseRunSolver ::
        ProjectConfig ->
        (Compiler, Platform, ProgramDb) ->
        [PackageSpecifier UnresolvedSourcePackage] ->
        Rebuild (SolverInstallPlan, PkgConfigDb, IndexUtils.TotalIndexState, IndexUtils.ActiveRepos)
      phaseRunSolver
        projectConfig@ProjectConfig
          { projectConfigShared,
            projectConfigBuildOnly
          }
        (compiler, platform, progdb)
        localPackages =
          rerunIfChanged
            verbosity
            fileMonitorSolverPlan
            ( solverSettings,
              localPackages,
              localPackagesEnabledStanzas,
              compiler,
              platform,
              programDbSignature progdb
            )
            $ do
              installedPkgIndex <-
                getInstalledPackages
                  verbosity
                  compiler
                  progdb
                  platform
                  corePackageDbs
              (sourcePkgDb, tis, ar) <-
                getSourcePackages
                  verbosity
                  withRepoCtx
                  (solverSettingIndexState solverSettings)
                  (solverSettingActiveRepos solverSettings)
              pkgConfigDB <- getPkgConfigDb verbosity progdb

              -- TODO: [code cleanup] it'd be better if the Compiler contained the
              -- ConfiguredPrograms that it needs, rather than relying on the progdb
              -- since we don't need to depend on all the programs here, just the
              -- ones relevant for the compiler.

              liftIO $ do
                solver <-
                  chooseSolver
                    verbosity
                    (solverSettingSolver solverSettings)
                    (compilerInfo compiler)

                Cabal.notice verbosity "Resolving dependencies..."
                planOrError <-
                  foldProgress logMsg (pure . Left) (pure . Right) $
                    planPackages
                      verbosity
                      compiler
                      platform
                      solver
                      solverSettings
                      installedPkgIndex
                      sourcePkgDb
                      pkgConfigDB
                      localPackages
                      localPackagesEnabledStanzas
                case planOrError of
                  Left msg -> do
                    reportPlanningFailure projectConfig compiler platform localPackages
                    Cabal.die' verbosity msg
                  Right plan -> return (plan, pkgConfigDB, tis, ar)
          where
            corePackageDbs :: [PackageDB]
            corePackageDbs =
              applyPackageDbFlags
                [GlobalPackageDB]
                (projectConfigPackageDBs projectConfigShared)

            withRepoCtx =
              projectConfigWithSolverRepoContext
                verbosity
                projectConfigShared
                projectConfigBuildOnly
            solverSettings = resolveSolverSettings projectConfig
            logMsg message rest = Cabal.debugNoWrap verbosity message >> rest

            localPackagesEnabledStanzas =
              Map.fromList
                [ (pkgname, stanzas)
                  | pkg <- localPackages,
                    -- TODO: misnomer: we should separate
                    -- builtin/global/inplace/local packages
                    -- and packages explicitly mentioned in the project
                    --
                    let pkgname = pkgSpecifierTarget pkg
                        testsEnabled =
                          lookupLocalPackageConfig
                            packageConfigTests
                            projectConfig
                            pkgname
                        benchmarksEnabled =
                          lookupLocalPackageConfig
                            packageConfigBenchmarks
                            projectConfig
                            pkgname
                        isLocal = isJust (shouldBeLocal pkg)
                        stanzas
                          | isLocal =
                              Map.fromList $
                                [ (TestStanzas, enabled)
                                  | enabled <- flagToList testsEnabled
                                ]
                                  ++ [ (BenchStanzas, enabled)
                                       | enabled <- flagToList benchmarksEnabled
                                     ]
                          | otherwise = Map.fromList [(TestStanzas, False), (BenchStanzas, False)]
                ]

      -- Elaborate the solver's install plan to get a fully detailed plan. This
      -- version of the plan has the final nix-style hashed ids.
      --
      phaseElaboratePlan ::
        ProjectConfig ->
        (Compiler, Platform, ProgramDb) ->
        PkgConfigDb ->
        SolverInstallPlan ->
        [PackageSpecifier (SourcePackage (PackageLocation loc))] ->
        Rebuild
          ( ElaboratedInstallPlan,
            ElaboratedSharedConfig
          )
      phaseElaboratePlan
        ProjectConfig
          { projectConfigShared,
            projectConfigAllPackages,
            projectConfigLocalPackages,
            projectConfigSpecificPackage,
            projectConfigBuildOnly
          }
        (compiler, platform, progdb)
        pkgConfigDB
        solverPlan
        localPackages = do
          liftIO $ Cabal.debug verbosity "Elaborating the install plan..."

          sourcePackageHashes <-
            rerunIfChanged
              verbosity
              fileMonitorSourceHashes
              (packageLocationsSignature solverPlan)
              $ getPackageSourceHashes verbosity withRepoCtx solverPlan

          defaultInstallDirs <- liftIO $ userInstallDirTemplates compiler
          (elaboratedPlan, elaboratedShared) <-
            liftIO . runLogProgress verbosity $
              elaborateInstallPlan
                verbosity
                platform
                compiler
                progdb
                pkgConfigDB
                distDirLayout
                cabalStoreDirLayout
                solverPlan
                localPackages
                sourcePackageHashes
                defaultInstallDirs
                projectConfigShared
                projectConfigAllPackages
                projectConfigLocalPackages
                (getMapMappend projectConfigSpecificPackage)
          let instantiatedPlan =
                instantiateInstallPlan
                  cabalStoreDirLayout
                  defaultInstallDirs
                  elaboratedShared
                  elaboratedPlan
          liftIO $ Cabal.debugNoWrap verbosity (InstallPlan.showInstallPlan instantiatedPlan)
          return (instantiatedPlan, elaboratedShared)
          where
            withRepoCtx =
              projectConfigWithSolverRepoContext
                verbosity
                projectConfigShared
                projectConfigBuildOnly

      -- Update the files we maintain that reflect our current build environment.
      -- In particular we maintain a JSON representation of the elaborated
      -- install plan (but not the improved plan since that reflects the state
      -- of the build rather than just the input environment).
      --
      phaseMaintainPlanOutputs ::
        ElaboratedInstallPlan ->
        ElaboratedSharedConfig ->
        Rebuild ()
      phaseMaintainPlanOutputs elaboratedPlan elaboratedShared = liftIO $ do
        Cabal.debug verbosity "Updating plan.json"
        writePlanExternalRepresentation
          distDirLayout
          elaboratedPlan
          elaboratedShared

      -- Improve the elaborated install plan. The elaborated plan consists
      -- mostly of source packages (with full nix-style hashed ids). Where
      -- corresponding installed packages already exist in the store, replace
      -- them in the plan.
      --
      -- Note that we do monitor the store's package db here, so we will redo
      -- this improvement phase when the db changes -- including as a result of
      -- executing a plan and installing things.
      --
      phaseImprovePlan ::
        ElaboratedInstallPlan ->
        ElaboratedSharedConfig ->
        Rebuild ElaboratedInstallPlan
      phaseImprovePlan elaboratedPlan elaboratedShared = do
        liftIO $ Cabal.debug verbosity "Improving the install plan..."
        storePkgIdSet <- getStoreEntries cabalStoreDirLayout compid
        let improvedPlan =
              improveInstallPlanWithInstalledPackages
                storePkgIdSet
                elaboratedPlan
        liftIO $ Cabal.debugNoWrap verbosity (InstallPlan.showInstallPlan improvedPlan)
        -- TODO: [nice to have] having checked which packages from the store
        -- we're using, it may be sensible to sanity check those packages
        -- by loading up the compiler package db and checking everything
        -- matches up as expected, e.g. no dangling deps, files deleted.
        return improvedPlan
        where
          compid = compilerId (pkgConfigCompiler elaboratedShared)
