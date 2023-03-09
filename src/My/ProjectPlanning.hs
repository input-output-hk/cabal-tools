module My.ProjectPlanning where

import Control.Monad.State as State (MonadIO (liftIO))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Client.Compat.Prelude (Verbosity, isJust)
import Distribution.Client.Dependency (PackageSpecifier, chooseSolver, foldProgress)
import Distribution.Client.DistDirLayout (CabalDirLayout (..), DistDirLayout (..))
import Distribution.Client.HttpUtils (HttpTransport)
import Distribution.Client.IndexUtils qualified as IndexUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.ProjectConfig
  ( checkBadPerPackageCompilerPaths,
    fetchAndReadSourcePackages,
    findProjectPackages,
    lookupLocalPackageConfig,
    projectConfigWithSolverRepoContext,
    readProjectConfig,
    resolveSolverSettings,
  )
import Distribution.Client.ProjectConfig.Legacy (instantiateProjectConfigSkeleton)
import Distribution.Client.ProjectConfig.Types
  ( MapMappend (getMapMappend),
    PackageConfig (packageConfigBenchmarks, packageConfigTests),
    ProjectConfig
      ( ProjectConfig,
        projectConfigAllPackages,
        projectConfigBuildOnly,
        projectConfigLocalPackages,
        projectConfigProvenance,
        projectConfigShared,
        projectConfigSpecificPackage
      ),
    ProjectConfigProvenance (Explicit),
    ProjectConfigShared
      ( projectConfigHcFlavor,
        projectConfigHcPath,
        projectConfigHcPkg,
        projectConfigPackageDBs
      ),
    SolverSettings
      ( solverSettingActiveRepos,
        solverSettingIndexState,
        solverSettingSolver
      ),
  )
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan)
import Distribution.Client.ProjectPlanning.Types (ElaboratedSharedConfig, SolverInstallPlan)
import Distribution.Client.RebuildMonad (Rebuild, newFileMonitor, rerunIfChanged, runRebuild)
import Distribution.Client.Store (StoreDirLayout)
import Distribution.Client.Types (PackageLocation, UnresolvedSourcePackage, pkgSpecifierTarget)
import Distribution.PackageDescription (ignoreConditions)
import Distribution.Simple.Compiler (Compiler, PackageDB (GlobalPackageDB), compilerInfo)
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Program.Db (configuredPrograms)
import Distribution.Simple.Program.Find (getSystemSearchPath)
import Distribution.Simple.Setup (Flag (..), flagToList)
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (BenchStanzas, TestStanzas))
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.SourcePackage (SourcePackage)
import Distribution.System (Platform (..))
import Distribution.Utils.LogProgress (runLogProgress)
import Original.Distribution.Client.ProjectPlanning (applyPackageDbFlags, elaborateInstallPlan, getInstalledPackages, getPackageSourceHashes, getPkgConfigDb, getSourcePackages, instantiateInstallPlan, packageLocationsSignature, planPackages, programDbSignature, reportPlanningFailure, shouldBeLocal, userInstallDirTemplates)

rebuildProjectConfig ::
  Verbosity ->
  Compiler ->
  Platform ->
  HttpTransport ->
  DistDirLayout ->
  Flag Bool ->
  Flag FilePath ->
  IO
    ( ProjectConfig,
      [PackageSpecifier UnresolvedSourcePackage]
    )
rebuildProjectConfig
  verbosity
  compiler
  (Platform arch os)
  httpTransport
  distDirLayout@DistDirLayout {distProjectRootDirectory}
  ignoreProject
  configFile =
    do
      (projectConfig, localPackages) <-
        runRebuild distProjectRootDirectory $ do
          projectConfigSkeleton <-
            readProjectConfig verbosity httpTransport ignoreProject configFile distDirLayout

          let projectConfig' = fst $ ignoreConditions projectConfigSkeleton

          liftIO $ do
            -- FIXME somehow hcFlavor is always set, I don't know who or what sets it
            case projectConfigHcFlavor $ projectConfigShared projectConfig' of
              Flag hcFlavor -> Cabal.warn verbosity $ "ignoring " ++ show hcFlavor ++ " in project config"
              _ -> pure ()

            case projectConfigHcPath $ projectConfigShared projectConfig' of
              Flag hcPath -> Cabal.warn verbosity $ "ignoring " ++ hcPath ++ " in project config"
              _ -> pure ()

            case projectConfigHcPkg $ projectConfigShared projectConfig' of
              Flag hcPkg -> Cabal.warn verbosity $ "ignoring " ++ hcPkg ++ " in project config"
              _ -> pure ()

          let projectConfig = instantiateProjectConfigSkeleton os arch (compilerInfo compiler) mempty projectConfigSkeleton

          localPackages <- readLocalPackages verbosity distDirLayout projectConfig

          return (projectConfig, localPackages)

      putStrLn $
        unlines $
          ("this build was affected by the following (project) config files:" :) $
            [ "- " ++ path
              | Explicit path <- Set.toList $ projectConfigProvenance projectConfig
            ]

      return (projectConfig, localPackages)

readLocalPackages ::
  Verbosity ->
  DistDirLayout ->
  ProjectConfig ->
  Rebuild [PackageSpecifier UnresolvedSourcePackage]
readLocalPackages verbosity distDirLayout projectConfig = do
  pkgLocations <- findProjectPackages distDirLayout projectConfig

  liftIO $ do
    Cabal.createDirectoryIfMissingVerbose verbosity True (distDirectory distDirLayout)
    Cabal.createDirectoryIfMissingVerbose verbosity True (distProjectCacheDirectory distDirLayout)

  fetchAndReadSourcePackages
    verbosity
    distDirLayout
    (projectConfigShared projectConfig)
    (projectConfigBuildOnly projectConfig)
    pkgLocations

rebuildInstallPlan ::
  Verbosity ->
  DistDirLayout ->
  CabalDirLayout ->
  ProjectConfig ->
  Compiler ->
  Platform ->
  ProgramDb ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  IO
    ( ElaboratedInstallPlan,
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
    }
  projectConfig
  compiler
  platform
  programDb
  localPackages =
    runRebuild distProjectRootDirectory $ do
      progsearchpath <- liftIO getSystemSearchPath
      let projectConfigMonitored = projectConfig {projectConfigBuildOnly = mempty}

      rerunIfChanged
        verbosity
        fileMonitorElaboratedPlan
        ( projectConfigMonitored,
          localPackages,
          progsearchpath
        )
        $ do
          -- Users are allowed to specify program locations independently for
          -- each package (e.g. to use a particular version of a pre-processor
          -- for some packages). However they cannot do this for the compiler
          -- itself as that's just not going to work. So we check for this.
          liftIO $
            checkBadPerPackageCompilerPaths
              (configuredPrograms programDb)
              (getMapMappend (projectConfigSpecificPackage projectConfig))

          (solverPlan, pkgConfigDB, totalIndexState, activeRepos) <-
            phaseRunSolver verbosity distDirLayout projectConfig (compiler, platform, programDb) localPackages

          (elaboratedPlan, elaboratedShared) <-
            phaseElaboratePlan
              verbosity
              cabalStoreDirLayout
              distDirLayout
              projectConfig
              (compiler, platform, programDb)
              pkgConfigDB
              solverPlan
              localPackages

          return (elaboratedPlan, elaboratedShared, totalIndexState, activeRepos)
    where
      fileMonitorElaboratedPlan = (newFileMonitor . distProjectCacheFile) "elaborated-plan"

-- Run the solver to get the initial install plan.
-- This is expensive so we cache it independently.
--
phaseRunSolver ::
  Verbosity ->
  DistDirLayout ->
  ProjectConfig ->
  (Compiler, Platform, ProgramDb) ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  Rebuild (SolverInstallPlan, PkgConfigDb, IndexUtils.TotalIndexState, IndexUtils.ActiveRepos)
phaseRunSolver
  verbosity
  DistDirLayout {distProjectCacheFile}
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
      fileMonitorSolverPlan = (newFileMonitor . distProjectCacheFile) "solver-plan"
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
  Verbosity ->
  StoreDirLayout ->
  DistDirLayout ->
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
  verbosity
  cabalStoreDirLayout
  distDirLayout@DistDirLayout {distProjectCacheFile}
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
      fileMonitorSourceHashes = (newFileMonitor . distProjectCacheFile) "source-hashes"
      withRepoCtx =
        projectConfigWithSolverRepoContext
          verbosity
          projectConfigShared
          projectConfigBuildOnly
