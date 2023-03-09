module My.ProjectPlanning where

import Control.Monad.State as State (MonadIO (liftIO))
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Client.Compat.Prelude (Verbosity, isJust)
import Distribution.Client.Dependency (PackageSpecifier, chooseSolver, foldProgress)
import Distribution.Client.DistDirLayout (CabalDirLayout (..), DistDirLayout (..))
import Distribution.Client.FetchUtils (checkRepoTarballFetched, fetchRepoTarball)
import Distribution.Client.GlobalFlags (RepoContext (..))
import Distribution.Client.HashValue (hashFromTUF, readFileHashValue)
import Distribution.Client.HttpUtils (HttpTransport)
import Distribution.Client.IndexUtils qualified as IndexUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.PackageHash (PackageSourceHash)
import Distribution.Client.ProjectConfig
  ( ProjectConfigShared (projectConfigActiveRepos, projectConfigIndexState),
    checkBadPerPackageCompilerPaths,
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
      ( solverSettingSolver
      ),
  )
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan)
import Distribution.Client.ProjectPlanning.Types (ElaboratedSharedConfig, SolverInstallPlan)
import Distribution.Client.RebuildMonad (Rebuild, monitorFile, monitorFiles, newFileMonitor, rerunIfChanged, runRebuild)
import Distribution.Client.Store (StoreDirLayout)
import Distribution.Client.Types (SourcePackageDb (packageIndex), pkgSpecifierTarget)
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.Repo (RemoteRepo (..), Repo (..))
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription (ignoreConditions)
import Distribution.Simple.Compiler (Compiler, PackageDB (GlobalPackageDB), compilerInfo)
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Program.Db (configuredPrograms)
import Distribution.Simple.Program.Find (getSystemSearchPath)
import Distribution.Simple.Setup (Flag (..), flagToList, flagToMaybe)
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (BenchStanzas, TestStanzas))
import Distribution.Solver.Types.PackageIndex qualified as PackageIndex
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform (..))
import Distribution.Types.PackageId (PackageId)
import Distribution.Utils.LogProgress (runLogProgress)
import Hackage.Security.Client qualified as Sec
import Original.Distribution.Client.ProjectPlanning (applyPackageDbFlags, elaborateInstallPlan, getInstalledPackages, getPkgConfigDb, getSourcePackages, instantiateInstallPlan, planPackages, programDbSignature, reportPlanningFailure, shouldBeLocal, userInstallDirTemplates)

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
  distDirLayout@DistDirLayout {distProjectRootDirectory}
  cabalDirLayout
  projectConfig@ProjectConfig {projectConfigShared, projectConfigBuildOnly}
  compiler
  platform
  programDb
  localPackages =
    runRebuild distProjectRootDirectory $ do
      (sourcePkgDb, tis, ar) <-
        getSourcePackages
          verbosity
          withRepoCtx
          (flagToMaybe $ projectConfigIndexState projectConfigShared)
          (flagToMaybe $ projectConfigActiveRepos projectConfigShared)

      -- approximate but close enough
      let packageLocationsSignatures =
            [ (packageId pkg, srcpkgSource pkg)
              | pkg <- PackageIndex.allPackages $ packageIndex sourcePkgDb
            ]

      sourcePackageHashes <-
        rerunIfChanged verbosity fileMonitorSourceHashes packageLocationsSignatures $
          getPackageSourceHashes verbosity withRepoCtx sourcePkgDb

      (elaboratedPlan, elaboratedShared) <-
        rebuildInstallPlanFromSourcePackageDb
          verbosity
          distDirLayout
          cabalDirLayout
          projectConfig
          compiler
          platform
          programDb
          sourcePkgDb
          sourcePackageHashes
          localPackages

      return (elaboratedPlan, elaboratedShared, tis, ar)
    where
      withRepoCtx =
        projectConfigWithSolverRepoContext
          verbosity
          projectConfigShared
          projectConfigBuildOnly
      fileMonitorSourceHashes = newFileMonitorInCacheDir "source-hashes"
      newFileMonitorInCacheDir = newFileMonitor . distProjectCacheFile distDirLayout

rebuildInstallPlanFromSourcePackageDb ::
  Verbosity ->
  DistDirLayout ->
  CabalDirLayout ->
  ProjectConfig ->
  Compiler ->
  Platform ->
  ProgramDb ->
  SourcePackageDb ->
  Map PackageId PackageSourceHash ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  Rebuild
    ( ElaboratedInstallPlan,
      ElaboratedSharedConfig
    )
rebuildInstallPlanFromSourcePackageDb
  verbosity
  distDirLayout@DistDirLayout {distProjectCacheFile}
  CabalDirLayout {cabalStoreDirLayout}
  projectConfig
  compiler
  platform
  programDb
  sourcePkgDb
  sourcePackageHashes
  localPackages = do
    progsearchpath <- liftIO getSystemSearchPath
    let projectConfigMonitored = projectConfig {projectConfigBuildOnly = mempty}

    rerunIfChanged
      verbosity
      fileMonitorElaboratedPlan
      (projectConfigMonitored, localPackages, progsearchpath)
      $ do
        -- Users are allowed to specify program locations independently for
        -- each package (e.g. to use a particular version of a pre-processor
        -- for some packages). However they cannot do this for the compiler
        -- itself as that's just not going to work. So we check for this.
        liftIO $
          checkBadPerPackageCompilerPaths
            (configuredPrograms programDb)
            (getMapMappend (projectConfigSpecificPackage projectConfig))

        (solverPlan, pkgConfigDB) <-
          phaseRunSolver verbosity distDirLayout projectConfig (compiler, platform, programDb) sourcePkgDb localPackages

        (elaboratedPlan, elaboratedShared) <-
          phaseElaboratePlan
            verbosity
            cabalStoreDirLayout
            distDirLayout
            projectConfig
            (compiler, platform, programDb)
            pkgConfigDB
            solverPlan
            sourcePackageHashes
            localPackages

        return (elaboratedPlan, elaboratedShared)
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
  SourcePackageDb ->
  [PackageSpecifier UnresolvedSourcePackage] ->
  Rebuild (SolverInstallPlan, PkgConfigDb)
phaseRunSolver
  verbosity
  DistDirLayout {distProjectCacheFile}
  projectConfig@ProjectConfig {projectConfigShared}
  (compiler, platform, progdb)
  sourcePkgDb
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
            Right plan -> return (plan, pkgConfigDB)
    where
      fileMonitorSolverPlan = (newFileMonitor . distProjectCacheFile) "solver-plan"
      corePackageDbs :: [PackageDB]
      corePackageDbs =
        applyPackageDbFlags
          [GlobalPackageDB]
          (projectConfigPackageDBs projectConfigShared)

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
  Map PackageId PackageSourceHash ->
  [PackageSpecifier (SourcePackage (PackageLocation loc))] ->
  Rebuild
    ( ElaboratedInstallPlan,
      ElaboratedSharedConfig
    )
phaseElaboratePlan
  verbosity
  cabalStoreDirLayout
  distDirLayout
  ProjectConfig
    { projectConfigShared,
      projectConfigAllPackages,
      projectConfigLocalPackages,
      projectConfigSpecificPackage
    }
  (compiler, platform, progdb)
  pkgConfigDB
  solverPlan
  sourcePackageHashes
  localPackages = do
    liftIO $ Cabal.debug verbosity "Elaborating the install plan..."

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

-- | Get the 'HashValue' for all the source packages where we use hashes,
-- and download any packages required to do so.
--
-- Note that we don't get hashes for local unpacked packages.
getPackageSourceHashes ::
  Verbosity ->
  (forall a. (RepoContext -> IO a) -> IO a) ->
  SourcePackageDb ->
  Rebuild (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity withRepoCtx sourcePkgDb = do
  -- Determine if and where to get the package's source hash from.
  --
  let allPkgLocations :: [(PackageId, PackageLocation (Maybe FilePath))]
      allPkgLocations =
        [ (packageId pkg, srcpkgSource pkg)
          | pkg <- PackageIndex.allPackages $ packageIndex sourcePkgDb
        ]

      -- Tarballs that were local in the first place.
      -- We'll hash these tarball files directly.
      localTarballPkgs :: [(PackageId, FilePath)]
      localTarballPkgs =
        [ (pkgid, tarball)
          | (pkgid, LocalTarballPackage tarball) <- allPkgLocations
        ]

      -- Tarballs from remote URLs. We must have downloaded these already
      -- (since we extracted the .cabal file earlier)
      remoteTarballPkgs =
        [ (pkgid, tarball)
          | (pkgid, RemoteTarballPackage _ (Just tarball)) <- allPkgLocations
        ]

      -- tarballs from source-repository-package stanzas
      sourceRepoTarballPkgs =
        [ (pkgid, tarball)
          | (pkgid, RemoteSourceRepoPackage _ (Just tarball)) <- allPkgLocations
        ]

      -- Tarballs from repositories, either where the repository provides
      -- hashes as part of the repo metadata, or where we will have to
      -- download and hash the tarball.
      repoTarballPkgsWithMetadata :: [(PackageId, Repo)]
      repoTarballPkgsWithoutMetadata :: [(PackageId, Repo)]
      (repoTarballPkgsWithMetadata, repoTarballPkgsWithoutMetadata) =
        partitionEithers
          [ case repo of
              RepoSecure {} -> Left (pkgid, repo)
              _ -> Right (pkgid, repo)
            | (pkgid, RepoTarballPackage repo _ _) <- allPkgLocations
          ]

  -- For tarballs from repos that do not have hashes available we now have
  -- to check if the packages were downloaded already.
  --
  (repoTarballPkgsToDownload, repoTarballPkgsDownloaded) <-
    fmap partitionEithers $
      liftIO $
        sequence
          [ do
              mtarball <- checkRepoTarballFetched repo pkgid
              case mtarball of
                Nothing -> return (Left (pkgid, repo))
                Just tarball -> return (Right (pkgid, tarball))
            | (pkgid, repo) <- repoTarballPkgsWithoutMetadata
          ]

  (hashesFromRepoMetadata, repoTarballPkgsNewlyDownloaded) <-
    -- Avoid having to initialise the repository (ie 'withRepoCtx') if we
    -- don't have to. (The main cost is configuring the http client.)
    if null repoTarballPkgsToDownload && null repoTarballPkgsWithMetadata
      then return (Map.empty, [])
      else liftIO $ withRepoCtx $ \repoctx -> do
        -- For tarballs from repos that do have hashes available as part of the
        -- repo metadata we now load up the index for each repo and retrieve
        -- the hashes for the packages
        --
        hashesFromRepoMetadata <-
          Sec.uncheckClientErrors $ -- TODO: [code cleanup] wrap in our own exceptions
          -- TODO: [code cleanup] wrap in our own exceptions
            Map.fromList . concat
              <$> sequence
                -- Reading the repo index is expensive so we group the packages by repo
                [ repoContextWithSecureRepo repoctx repo $ \secureRepo ->
                    Sec.withIndex secureRepo $ \repoIndex ->
                      sequence
                        [ do
                            hash <-
                              Sec.trusted
                                <$> Sec.indexLookupHash repoIndex pkgid -- strip off Trusted tag

                            -- Note that hackage-security currently uses SHA256
                            -- but this API could in principle give us some other
                            -- choice in future.
                            return (pkgid, hashFromTUF hash)
                          | pkgid <- pkgids
                        ]
                  | (repo, pkgids) <-
                      map (\grp@((_, repo) :| _) -> (repo, map fst (NE.toList grp)))
                        . NE.groupBy ((==) `on` (remoteRepoName . repoRemote . snd))
                        . sortBy (compare `on` (remoteRepoName . repoRemote . snd))
                        $ repoTarballPkgsWithMetadata
                ]

        -- For tarballs from repos that do not have hashes available, download
        -- the ones we previously determined we need.
        --
        repoTarballPkgsNewlyDownloaded <-
          sequence
            [ do
                tarball <- fetchRepoTarball verbosity repoctx repo pkgid
                return (pkgid, tarball)
              | (pkgid, repo) <- repoTarballPkgsToDownload
            ]

        return (hashesFromRepoMetadata, repoTarballPkgsNewlyDownloaded)

  -- Hash tarball files for packages where we have to do that. This includes
  -- tarballs that were local in the first place, plus tarballs from repos,
  -- either previously cached or freshly downloaded.
  --
  let allTarballFilePkgs :: [(PackageId, FilePath)]
      allTarballFilePkgs =
        localTarballPkgs
          ++ remoteTarballPkgs
          ++ sourceRepoTarballPkgs
          ++ repoTarballPkgsDownloaded
          ++ repoTarballPkgsNewlyDownloaded

  hashesFromTarballFiles <-
    liftIO
      ( Map.fromList
          <$> sequence
            [ do
                srchash <- readFileHashValue tarball
                return (pkgid, srchash)
              | (pkgid, tarball) <- allTarballFilePkgs
            ]
      )

  monitorFiles [monitorFile tarball | (_pkgid, tarball) <- allTarballFilePkgs]

  -- Return the combination
  return $! hashesFromRepoMetadata <> hashesFromTarballFiles
