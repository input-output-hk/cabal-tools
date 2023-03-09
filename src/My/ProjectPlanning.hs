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
import Data.Traversable (for)
import Distribution.Client.Compat.Prelude (Verbosity, isJust)
import Distribution.Client.Dependency (PackageSpecifier, foldProgress)
import Distribution.Client.Dependency.Types (Solver (Modular))
import Distribution.Client.DistDirLayout (DistDirLayout (..))
import Distribution.Client.FetchUtils (checkRepoTarballFetched, fetchRepoTarball)
import Distribution.Client.GlobalFlags (RepoContext (repoContextWithSecureRepo))
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
    ProjectConfig (..),
    ProjectConfigProvenance (Explicit),
    ProjectConfigShared
      ( projectConfigHcFlavor,
        projectConfigHcPath,
        projectConfigHcPkg,
        projectConfigPackageDBs
      ),
  )
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan)
import Distribution.Client.ProjectPlanning.Types (ElaboratedSharedConfig, SolverInstallPlan)
import Distribution.Client.RebuildMonad (Rebuild, runRebuild)
import Distribution.Client.SolverInstallPlan qualified as SolverInstallPlan
import Distribution.Client.Store (StoreDirLayout)
import Distribution.Client.Types (PackageLocation, UnresolvedSourcePackage, pkgSpecifierTarget)
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.Repo (RemoteRepo (..), Repo (..))
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription (ignoreConditions)
import Distribution.Simple.Compiler (Compiler, PackageDB (GlobalPackageDB), compilerInfo)
import Distribution.Simple.Flag (Flag (..), flagElim, flagToMaybe)
import Distribution.Simple.Program (ProgramDb)
import Distribution.Simple.Program.Db (configuredPrograms)
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (BenchStanzas, TestStanzas))
import Distribution.Solver.Types.PkgConfigDb (readPkgConfigDb)
import Distribution.Solver.Types.Progress (Progress)
import Distribution.Solver.Types.SolverPackage
  ( SolverPackage (SolverPackage, solverPkgSource),
  )
import Distribution.Solver.Types.SourcePackage (SourcePackage (..))
import Distribution.System (Platform (..))
import Distribution.Types.PackageId (PackageId)
import Distribution.Types.PackageName (PackageName)
import Distribution.Utils.LogProgress (runLogProgress)
import Hackage.Security.Client qualified as Sec
import Original.Distribution.Client.ProjectPlanning
  ( applyPackageDbFlags,
    elaborateInstallPlan,
    instantiateInstallPlan,
    planPackages,
    shouldBeLocal,
    userInstallDirTemplates,
  )

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
          "this build was affected by the following (project) config files:"
            : [ "- " ++ path
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
  StoreDirLayout ->
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
  distDirLayout
  cabalStoreDirLayout
  projectConfig
  compiler
  platform
  programDb
  localPackages = do
    -- Users are allowed to specify program locations independently for
    -- each package (e.g. to use a particular version of a pre-processor
    -- for some packages). However they cannot do this for the compiler
    -- itself as that's just not going to work. So we check for this.
    checkBadPerPackageCompilerPaths
      (configuredPrograms programDb)
      (getMapMappend (projectConfigSpecificPackage projectConfig))

    (sourcePkgDb, totalIndexState, activeRepos) <-
      withRepoCtx $ \repoCtx -> do
        IndexUtils.getSourcePackagesAtIndexState
          verbosity
          repoCtx
          (flagToMaybe $ projectConfigIndexState $ projectConfigShared projectConfig)
          (flagToMaybe $ projectConfigActiveRepos $ projectConfigShared projectConfig)

    installedPkgIndex <- IndexUtils.getInstalledPackages verbosity compiler corePackageDbs programDb

    pkgConfigDB <- readPkgConfigDb verbosity programDb

    -- TODO: [code cleanup] it'd be better if the Compiler contained the
    -- ConfiguredPrograms that it needs, rather than relying on the progdb
    -- since we don't need to depend on all the programs here, just the
    -- ones relevant for the compiler.

    Cabal.notice verbosity "Resolving dependencies..."

    solverPlan <-
      runProgress
        verbosity
        ( planPackages
            verbosity
            compiler
            platform
            Modular
            (resolveSolverSettings projectConfig)
            installedPkgIndex
            sourcePkgDb
            pkgConfigDB
            localPackages
            (computeLocalPackagesEnabledStanzas localPackages projectConfig)
        )
        >>= either (Cabal.die' verbosity) return

    sourcePackageHashes <- getPackageSourceHashes verbosity withRepoCtx solverPlan

    Cabal.debug verbosity "Elaborating the install plan..."

    defaultInstallDirs <- userInstallDirTemplates compiler

    (elaboratedPlan, elaboratedShared) <-
      runLogProgress verbosity $
        elaborateInstallPlan
          verbosity
          platform
          compiler
          programDb
          pkgConfigDB
          distDirLayout
          cabalStoreDirLayout
          solverPlan
          localPackages
          sourcePackageHashes
          defaultInstallDirs
          (projectConfigShared projectConfig)
          (projectConfigAllPackages projectConfig)
          (projectConfigLocalPackages projectConfig)
          (getMapMappend $ projectConfigSpecificPackage projectConfig)

    let instantiatedPlan =
          instantiateInstallPlan
            cabalStoreDirLayout
            defaultInstallDirs
            elaboratedShared
            elaboratedPlan

    Cabal.debugNoWrap verbosity (InstallPlan.showInstallPlan instantiatedPlan)

    return (instantiatedPlan, elaboratedShared, totalIndexState, activeRepos)
    where
      corePackageDbs =
        applyPackageDbFlags
          [GlobalPackageDB]
          (projectConfigPackageDBs $ projectConfigShared projectConfig)

      withRepoCtx =
        projectConfigWithSolverRepoContext
          verbosity
          (projectConfigShared projectConfig)
          (projectConfigBuildOnly projectConfig)

computeLocalPackagesEnabledStanzas ::
  [PackageSpecifier (SourcePackage (PackageLocation loc))] ->
  ProjectConfig ->
  Map PackageName (Map OptionalStanza Bool)
computeLocalPackagesEnabledStanzas localPackages projectConfig =
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
                  flagElim id (Map.insert TestStanzas) testsEnabled $
                    flagElim id (Map.insert BenchStanzas) benchmarksEnabled Map.empty
              | otherwise = Map.fromList [(TestStanzas, False), (BenchStanzas, False)]
    ]

-- Elaborate the-- | Get the 'HashValue' for all the source packages where we use hashes,
-- and download any packages required to do so.
--
-- Note that we don't get hashes for local unpacked packages.
getPackageSourceHashes ::
  Verbosity ->
  (forall a. (RepoContext -> IO a) -> IO a) ->
  SolverInstallPlan ->
  IO (Map PackageId PackageSourceHash)
getPackageSourceHashes verbosity withRepoCtx solverPlan = do
  -- Determine if and where to get the package's source hash from.

  let allPkgLocations :: [(PackageId, PackageLocation (Maybe FilePath))]
      allPkgLocations =
        [ (packageId pkg, srcpkgSource pkg)
          | SolverInstallPlan.Configured (SolverPackage {solverPkgSource = pkg}) <-
              SolverInstallPlan.toList solverPlan
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
  (repoTarballPkgsToDownload, repoTarballPkgsDownloaded) <-
    partitionEithers
      <$> for
        repoTarballPkgsWithoutMetadata
        ( \(pkgId, repo) -> do
            mtarball <- checkRepoTarballFetched repo pkgId
            return $ case mtarball of
              Nothing -> Left (pkgId, repo)
              Just tarball -> Right (pkgId, tarball)
        )

  (hashesFromRepoMetadata, repoTarballPkgsNewlyDownloaded) <-
    -- Avoid having to initialise the repository (ie 'withRepoCtx') if we
    -- don't have to. (The main cost is configuring the http client.)
    if null repoTarballPkgsToDownload && null repoTarballPkgsWithMetadata
      then return (Map.empty, [])
      else withRepoCtx $ \repoctx -> do
        -- For tarballs from repos that do have hashes available as part of the
        -- repo metadata we now load up the index for each repo and retrieve
        -- the hashes for the packages
        --
        hashesFromRepoMetadata <-
          -- Reading the repo index is expensive so we group the packages by repo
          let pkgIdsPerRepo =
                map (\grp@((_, repo) :| _) -> (repo, map fst (NE.toList grp)))
                  . NE.groupBy ((==) `on` (remoteRepoName . repoRemote . snd))
                  . sortBy (compare `on` (remoteRepoName . repoRemote . snd))
                  $ repoTarballPkgsWithMetadata
           in Sec.uncheckClientErrors $
                Map.fromList
                  <$> foldMap
                    ( \(repo, pkgids) ->
                        repoContextWithSecureRepo repoctx repo $ \secureRepo ->
                          Sec.withIndex secureRepo $ \repoIndex ->
                            for pkgids $ \pkgid -> do
                              hash <- Sec.trusted <$> Sec.indexLookupHash repoIndex pkgid -- strip off Trusted tag

                              -- Note that hackage-security currently uses SHA256
                              -- but this API could in principle give us some other
                              -- choice in future.
                              return (pkgid, hashFromTUF hash)
                    )
                    pkgIdsPerRepo

        -- For tarballs from repos that do not have hashes available, download
        -- the ones we previously determined we need.
        --
        repoTarballPkgsNewlyDownloaded <-
          for repoTarballPkgsToDownload $ \(pkgId, repo) -> do
            tarball <- fetchRepoTarball verbosity repoctx repo pkgId
            return (pkgId, tarball)

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
    Map.fromList
      <$> for
        allTarballFilePkgs
        ( \(pkgId, tarball) -> do
            srcHash <- readFileHashValue tarball
            return (pkgId, srcHash)
        )

  -- Return the combination
  return $! hashesFromRepoMetadata <> hashesFromTarballFiles

runProgress :: Verbosity -> Progress String fail b -> IO (Either fail b)
runProgress verbosity = foldProgress logMsg (pure . Left) (pure . Right)
  where
    logMsg message rest = Cabal.debugNoWrap verbosity message >> rest
