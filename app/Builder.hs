{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Builder.ProjectBuilding (buildAndInstallUnpackedPackage)
import Control.Exception
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Data.Foldable (for_, toList, traverse_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Distribution.Client.CmdBuild qualified as CmdBuild
import Distribution.Client.CmdErrorMessages
import Distribution.Client.Config
import Distribution.Client.DistDirLayout
import Distribution.Client.FetchUtils
import Distribution.Client.HttpUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.JobControl
import Distribution.Client.ProjectBuilding (BuildFailure (..), BuildFailureReason (..), BuildOutcomes, BuildResult (..), BuildStatus (..), BuildStatusMap)
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.ProjectOrchestration hiding (establishProjectBaseContext, establishProjectBaseContextWithRoot, runProjectBuildPhase)
import Distribution.Client.ProjectPlanning hiding (rebuildProjectConfig)
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.RebuildMonad
import Distribution.Client.Setup hiding (packageName)
import Distribution.Client.Tar qualified as Tar
import Distribution.Client.TargetProblem
import Distribution.Client.Targets
import Distribution.Client.Types.BuildResults (DocsResult (..), TestsResult (..))
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.PackageSpecifier
import Distribution.Client.Types.ReadyPackage
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.Package
import Distribution.PackageDescription qualified as PD
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (flagToMaybe, toFlag)
import Distribution.Simple.Program.Db
import Distribution.Simple.Register qualified as Cabal
import Distribution.Simple.Setup qualified as Setup
import Distribution.Simple.Utils qualified as Utils
import Distribution.Solver.Types.ConstraintSource
import Distribution.System
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.NubList
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import Options.Applicative qualified as Opt
import System.Directory (doesDirectoryExist, renameDirectory)
import System.Environment (getArgs)
import System.FilePath
import System.Process (callProcess)

data Args = Args
  { argNamedPackages :: [PackageVersionConstraint],
    argVerbosity :: Verbosity,
    argDryRun :: Bool,
    argConstraints :: [UserConstraint]
  }

argsParser :: Opt.Parser Args
argsParser =
  Args
    Opt.<$> Opt.some (Opt.argument (Opt.maybeReader simpleParsec) (Opt.help "The packages to build"))
    Opt.<*> Opt.option (Opt.maybeReader simpleParsec) (Opt.long "verbosity" <> Opt.short 'v' <> Opt.value Verbosity.normal <> Opt.help "Verbosity")
    Opt.<*> Opt.switch (Opt.long "dry-run" <> Opt.help "Only compute a build plan")
    Opt.<*> Opt.many (Opt.option (Opt.maybeReader simpleParsec) (Opt.long "constraint" <> Opt.help "Constraints"))

main :: IO ()
main = do
  args <- getArgs
  case args of
    "act-as-setup" : _ ->
      callProcess "cabal" args
    _otherwise ->
      build =<< Opt.execParser opts
  where
    opts =
      Opt.info
        (argsParser Opt.<**> Opt.helper)
        ( Opt.fullDesc
            <> Opt.progDesc "Builds a package"
            <> Opt.header "hello - a busy package builder"
        )

build :: Args -> IO ()
build Args {argNamedPackages, argVerbosity, argDryRun, argConstraints} = do
  let cliConfig =
        mempty
          { projectPackagesNamed = argNamedPackages,
            projectConfigBuildOnly = mempty {projectConfigDryRun = toFlag argDryRun},
            projectConfigShared =
              mempty
                { projectConfigConstraints = map (,ConstraintSourceCommandlineFlag) argConstraints
                }
          }

  projectRoot <- either throwIO return =<< findProjectRoot Nothing Nothing

  baseCtx <- establishProjectBaseContextWithRoot argVerbosity cliConfig projectRoot OtherCommand

  buildCtx <- runProjectPreBuildPhase argVerbosity baseCtx $ \elaboratedPlan -> do
    let targetSelectors = [TargetPackageNamed pkgName Nothing | PackageVersionConstraint pkgName _ <- argNamedPackages]

    targets <-
      either (reportBuildTargetProblems argVerbosity) return $
        resolveTargets
          CmdBuild.selectPackageTargets
          CmdBuild.selectComponentTarget
          elaboratedPlan
          Nothing
          targetSelectors

    for_ (Map.toList targets) $ \(unitId, cts) ->
      putStrLn $
        prettyShow unitId
          <> " "
          <> Utils.intercalate "," [prettyShow cn <> " " <> show sct <> ", selected by: " ++ Utils.intercalate "," (map showTargetSelector $ toList ts) | (ComponentTarget cn sct, ts) <- cts]

    return (elaboratedPlan, targets)

  printPlan argVerbosity baseCtx buildCtx

  buildOutcomes <- runProjectBuildPhase argVerbosity baseCtx buildCtx

  runProjectPostBuildPhase argVerbosity baseCtx buildCtx buildOutcomes

  putStrLn "done."

-- Copied the following from Distribution.Client.ProjectPlanning and
-- modified to always rebuild (because cabal logic does not take cli config
-- into account)

establishProjectBaseContextWithRoot ::
  Verbosity ->
  ProjectConfig ->
  ProjectRoot ->
  CurrentCommand ->
  IO ProjectBaseContext
establishProjectBaseContextWithRoot verbosity cliConfig projectRoot currentCommand = do
  cabalDir <- getCabalDir

  let mdistDirectory = Setup.flagToMaybe (projectConfigDistDir $ projectConfigShared cliConfig)
  let distDirLayout = defaultDistDirLayout projectRoot mdistDirectory

  httpTransport <-
    configureTransport
      verbosity
      (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
      (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      cliConfig

  let mlogsDir = Setup.flagToMaybe (projectConfigLogsDir $ projectConfigBuildOnly projectConfig)

  mstoreDir <- traverse makeAbsolute $ Setup.flagToMaybe (projectConfigStoreDir $ projectConfigShared projectConfig)

  let cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

  let buildSettings = resolveBuildTimeSettings verbosity cabalDirLayout projectConfig

  return
    ProjectBaseContext
      { distDirLayout,
        cabalDirLayout,
        projectConfig,
        localPackages,
        buildSettings,
        currentCommand
      }

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
  distDirLayout@DistDirLayout {distProjectRootDirectory, distProjectCacheDirectory}
  cliConfig =
    do
      (projectConfig, localPackages) <-
        runRebuild distProjectRootDirectory $ do
          liftIO $ Utils.createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

          projectConfigSkeleton <-
            readProjectConfig
              verbosity
              httpTransport
              (projectConfigIgnoreProject $ projectConfigShared cliConfig)
              (projectConfigConfigFile $ projectConfigShared cliConfig)
              distDirLayout

          let (projectConfig', _projectConfigImports) = PD.ignoreConditions projectConfigSkeleton

          (compiler, Platform arch os, _) <- configureCompiler verbosity distDirLayout (projectConfig' <> cliConfig)

          let projectConfig = instantiateProjectConfigSkeleton os arch (compilerInfo compiler) mempty projectConfigSkeleton

          localPackages <- readLocalPackages verbosity distDirLayout (projectConfig <> cliConfig)

          return (projectConfig, localPackages)

      putStrLn $
        unlines $
          ("this build was affected by the following (project) config files:" :) $
            [ "- " ++ path
              | Explicit path <- toList $ projectConfigProvenance projectConfig
            ]

      return (projectConfig <> cliConfig, localPackages)

readLocalPackages ::
  Verbosity ->
  DistDirLayout ->
  ProjectConfig ->
  Rebuild [PackageSpecifier UnresolvedSourcePackage]
readLocalPackages verbosity distDirLayout projectConfig = do
  pkgLocations <- findProjectPackages distDirLayout projectConfig

  liftIO $ do
    Utils.createDirectoryIfMissingVerbose verbosity True (distDirectory distDirLayout)
    Utils.createDirectoryIfMissingVerbose verbosity True (distProjectCacheDirectory distDirLayout)

  fetchAndReadSourcePackages
    verbosity
    distDirLayout
    (projectConfigShared projectConfig)
    (projectConfigBuildOnly projectConfig)
    pkgLocations

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity =
  reportTargetProblems verbosity "build"

-- | Build phase: now do it.
--
-- Execute all or parts of the description of what to do to build or
-- rebuild the various packages needed.
runProjectBuildPhase ::
  Verbosity ->
  ProjectBaseContext ->
  ProjectBuildContext ->
  IO BuildOutcomes
runProjectBuildPhase _verbosity ProjectBaseContext {buildSettings} _projectBuildContext
  | buildSettingDryRun buildSettings =
      return Map.empty
runProjectBuildPhase
  verbosity
  ProjectBaseContext {distDirLayout, cabalDirLayout, buildSettings}
  ProjectBuildContext
    { elaboratedPlanToExecute,
      elaboratedShared,
      pkgsBuildStatus
    } =
    Map.union (previousBuildOutcomes pkgsBuildStatus)
      Opt.<$> rebuildTargets
        verbosity
        distDirLayout
        (cabalStoreDirLayout cabalDirLayout)
        elaboratedPlanToExecute
        elaboratedShared
        pkgsBuildStatus
        buildSettings
    where
      previousBuildOutcomes :: BuildStatusMap -> BuildOutcomes
      previousBuildOutcomes =
        Map.mapMaybe $ \case
          BuildStatusUpToDate buildSuccess -> Just (Right buildSuccess)
          -- TODO: [nice to have] record build failures persistently
          _ -> Nothing

-- | Build things for real.
--
-- It requires the 'BuildStatusMap' gathered by 'rebuildTargetsDryRun'.
rebuildTargets ::
  Verbosity ->
  DistDirLayout ->
  StoreDirLayout ->
  ElaboratedInstallPlan ->
  ElaboratedSharedConfig ->
  BuildStatusMap ->
  BuildTimeSettings ->
  IO BuildOutcomes
rebuildTargets
  verbosity
  distDirLayout@DistDirLayout {..}
  storeDirLayout
  installPlan
  sharedPackageConfig@ElaboratedSharedConfig
    { pkgConfigCompiler = compiler,
      pkgConfigCompilerProgs = progdb
    }
  pkgsBuildStatus
  buildSettings@BuildTimeSettings
    { buildSettingNumJobs,
      buildSettingKeepGoing
    } = do
    -- Concurrency control: create the job controller and concurrency limits
    -- for downloading, building and installing.
    jobControl <-
      if isParallelBuild
        then newParallelJobControl buildSettingNumJobs
        else newSerialJobControl
    -- TODO: [code cleanup] eliminate setup exe cache
    Utils.debug verbosity $
      "Executing install plan "
        ++ if isParallelBuild
          then " in parallel using " ++ show buildSettingNumJobs ++ " threads."
          else " serially."

    Utils.createDirectoryIfMissingVerbose verbosity True distBuildRootDirectory
    Utils.createDirectoryIfMissingVerbose verbosity True distTempDirectory
    traverse_ (createPackageDBIfMissing verbosity compiler progdb) packageDBsToUse

    -- Before traversing the install plan, preemptively find all packages that
    -- will need to be downloaded and start downloading them.
    asyncDownloadPackages
      verbosity
      withRepoCtx
      installPlan
      pkgsBuildStatus
      $ \downloadMap ->
        -- For each package in the plan, in dependency order, but in parallel...
        InstallPlan.execute
          jobControl
          keepGoing
          (BuildFailure Nothing . DependentFailed . packageId)
          installPlan
          $ \pkg ->
            -- TODO: review exception handling
            handle (\(e :: BuildFailure) -> return (Left e)) $
              fmap Right $
                let uid = installedUnitId pkg
                    pkgBuildStatus = Map.findWithDefault (error "rebuildTargets") uid pkgsBuildStatus
                 in rebuildTarget
                      verbosity
                      distDirLayout
                      storeDirLayout
                      buildSettings
                      downloadMap
                      sharedPackageConfig
                      pkg
                      pkgBuildStatus
    where
      isParallelBuild = buildSettingNumJobs >= 2
      keepGoing = buildSettingKeepGoing
      withRepoCtx =
        projectConfigWithBuilderRepoContext
          verbosity
          buildSettings
      packageDBsToUse =
        -- all the package dbs we may need to create
        (Set.toList . Set.fromList)
          [ pkgdb
            | InstallPlan.Configured elab <- InstallPlan.toList installPlan,
              pkgdb <-
                concat
                  [ elabBuildPackageDBStack elab,
                    elabRegisterPackageDBStack elab,
                    elabSetupPackageDBStack elab
                  ]
          ]

-- | Given all the context and resources, (re)build an individual package.
rebuildTarget ::
  Verbosity ->
  DistDirLayout ->
  StoreDirLayout ->
  BuildTimeSettings ->
  AsyncFetchMap ->
  ElaboratedSharedConfig ->
  ElaboratedReadyPackage ->
  BuildStatus ->
  IO BuildResult
rebuildTarget
  verbosity
  distDirLayout
  storeDirLayout
  buildSettings@BuildTimeSettings {buildSettingLogFile}
  downloadMap
  sharedPackageConfig@ElaboratedSharedConfig {pkgConfigPlatform, pkgConfigCompiler}
  (ReadyPackage pkg)
  pkgBuildStatus
    -- Technically, doing the --only-download filtering only in this function is
    -- not perfect. We could also prune the plan at an earlier stage, like it's
    -- done with --only-dependencies. But...
    --   * the benefit would be minimal (practically just avoiding to print the
    --     "requires build" parts of the plan)
    --   * we currently don't have easy access to the BuildStatus of packages
    --     in the pruning phase
    --   * we still have to check it here to avoid performing successive phases
    | buildSettingOnlyDownload buildSettings = do
        case pkgBuildStatus of
          BuildStatusDownload ->
            void $ waitAsyncPackageDownload verbosity downloadMap pkg
          _ -> return ()
        return $ BuildResult DocsNotTried TestsNotTried Nothing
    | otherwise =
        -- We rely on the 'BuildStatus' to decide which phase to start from:
        case pkgBuildStatus of
          BuildStatusDownload -> downloadPhase
          BuildStatusUnpack tarball -> unpackTarballPhase tarball
          BuildStatusRebuild {} -> fail "not supported"
          -- TODO: perhaps re-nest the types to make these impossible
          BuildStatusPreExisting {} -> unexpectedState
          BuildStatusInstalled {} -> unexpectedState
          BuildStatusUpToDate {} -> unexpectedState
    where
      unexpectedState = error "rebuildTarget: unexpected package status"

      downloadPhase :: IO BuildResult
      downloadPhase = do
        downsrcloc <-
          waitAsyncPackageDownload verbosity downloadMap pkg
        case downsrcloc of
          DownloadedTarball tarball -> unpackTarballPhase tarball
      -- TODO: [nice to have] git/darcs repos etc

      unpackTarballPhase :: FilePath -> IO BuildResult
      unpackTarballPhase tarball =
        withTarballLocalDirectory
          verbosity
          distDirLayout
          tarball
          (packageId pkg)
          (elabDistDirParams sharedPackageConfig pkg)
          (elabBuildStyle pkg)
          (elabPkgDescriptionOverride pkg)
          $ case elabBuildStyle pkg of
            BuildAndInstall -> buildAndInstall
            BuildInplaceOnly -> \_ _ -> fail "not supported"

      buildAndInstall :: FilePath -> FilePath -> IO BuildResult
      buildAndInstall srcdir builddir =
        buildAndInstallUnpackedPackage
          verbosity
          storeDirLayout
          sharedPackageConfig
          pkg
          builddir'
          mlogFile
        where
          builddir' = makeRelative srcdir builddir
      -- TODO: [nice to have] ^^ do this relative stuff better

      mlogFile :: Maybe FilePath
      mlogFile =
        case buildSettingLogFile of
          Nothing -> Nothing
          Just mkLogFile -> Just (mkLogFile pkgConfigCompiler pkgConfigPlatform (packageId pkg) (installedUnitId pkg))

newtype DownloadedSourceLocation = DownloadedTarball FilePath

downloadedSourceLocation ::
  PackageLocation FilePath ->
  Maybe DownloadedSourceLocation
downloadedSourceLocation pkgloc =
  case pkgloc of
    RemoteTarballPackage _ tarball -> Just (DownloadedTarball tarball)
    RepoTarballPackage _ _ tarball -> Just (DownloadedTarball tarball)
    _ -> Nothing

-- | Check if a package needs downloading, and if so expect to find a download
-- in progress in the given 'AsyncFetchMap' and wait on it to finish.
waitAsyncPackageDownload ::
  Verbosity ->
  AsyncFetchMap ->
  ElaboratedConfiguredPackage ->
  IO DownloadedSourceLocation
waitAsyncPackageDownload verbosity downloadMap elab = do
  pkgloc <-
    waitAsyncFetchPackage
      verbosity
      downloadMap
      (elabPkgSourceLocation elab)
  case downloadedSourceLocation pkgloc of
    Just loc -> return loc
    Nothing -> fail "waitAsyncPackageDownload: unexpected source location"

-- TODO: [nice to have] do we need to use a with-style for the temp
-- files for downloading http packages, or are we going to cache them
-- persistently?

-- | Given the current 'InstallPlan' and 'BuildStatusMap', select all the
-- packages we have to download and fork off an async action to download them.
-- We download them in dependency order so that the one's we'll need
-- first are the ones we will start downloading first.
--
-- The body action is passed a map from those packages (identified by their
-- location) to a completion var for that package. So the body action should
-- lookup the location and use 'waitAsyncPackageDownload' to get the result.
asyncDownloadPackages ::
  Verbosity ->
  ((RepoContext -> IO a) -> IO a) ->
  ElaboratedInstallPlan ->
  BuildStatusMap ->
  (AsyncFetchMap -> IO a) ->
  IO a
asyncDownloadPackages verbosity withRepoCtx installPlan pkgsBuildStatus body
  | null pkgsToDownload = body Map.empty
  | otherwise = withRepoCtx $ \repoctx ->
      asyncFetchPackages
        verbosity
        repoctx
        pkgsToDownload
        body
  where
    pkgsToDownload :: [PackageLocation (Maybe FilePath)]
    pkgsToDownload =
      Utils.ordNub $
        [ elabPkgSourceLocation elab
          | InstallPlan.Configured elab <-
              InstallPlan.reverseTopologicalOrder installPlan,
            let uid = installedUnitId elab
                pkgBuildStatus = Map.findWithDefault (error "asyncDownloadPackages") uid pkgsBuildStatus,
            BuildStatusDownload <- [pkgBuildStatus]
        ]

-- | Create a package DB if it does not currently exist. Note that this action
-- is /not/ safe to run concurrently.
createPackageDBIfMissing ::
  Verbosity ->
  Compiler ->
  ProgramDb ->
  PackageDB ->
  IO ()
createPackageDBIfMissing
  verbosity
  compiler
  progdb
  (SpecificPackageDB dbPath) = do
    exists <- Cabal.doesPackageDBExist dbPath
    unless exists $ do
      Utils.createDirectoryIfMissingVerbose verbosity True (takeDirectory dbPath)
      Cabal.createPackageDB verbosity compiler progdb False dbPath
createPackageDBIfMissing _ _ _ _ = return ()

-- | Ensure that the package is unpacked in an appropriate directory, either
-- a temporary one or a persistent one under the shared dist directory.
withTarballLocalDirectory ::
  Verbosity ->
  DistDirLayout ->
  FilePath ->
  PackageId ->
  DistDirParams ->
  BuildStyle ->
  Maybe CabalFileText ->
  ( FilePath -> -- Source directory
    FilePath -> -- Build directory
    IO a
  ) ->
  IO a
withTarballLocalDirectory
  verbosity
  distDirLayout@DistDirLayout {..}
  tarball
  pkgid
  dparams
  buildstyle
  pkgTextOverride
  buildPkg =
    case buildstyle of
      -- In this case we make a temp dir (e.g. tmp/src2345/), unpack
      -- the tarball to it (e.g. tmp/src2345/foo-1.0/), and for
      -- compatibility we put the dist dir within it
      -- (i.e. tmp/src2345/foo-1.0/dist/).
      --
      -- Unfortunately, a few custom Setup.hs scripts do not respect
      -- the --builddir flag and always look for it at ./dist/ so
      -- this way we avoid breaking those packages
      BuildAndInstall ->
        let tmpdir = distTempDirectory
         in Utils.withTempDirectory verbosity tmpdir "src" $ \unpackdir -> do
              unpackPackageTarball
                verbosity
                tarball
                unpackdir
                pkgid
                pkgTextOverride
              let srcdir = unpackdir </> prettyShow pkgid
                  builddir = srcdir </> "dist"
              buildPkg srcdir builddir

      -- In this case we make sure the tarball has been unpacked to the
      -- appropriate location under the shared dist dir, and then build it
      -- inplace there
      BuildInplaceOnly -> do
        let srcrootdir = distUnpackedSrcRootDirectory
            srcdir = distUnpackedSrcDirectory pkgid
            builddir = distBuildDirectory dparams
        -- TODO: [nice to have] use a proper file monitor rather
        -- than this dir exists test
        exists <- doesDirectoryExist srcdir
        unless exists $ do
          Utils.createDirectoryIfMissingVerbose verbosity True srcrootdir
          unpackPackageTarball
            verbosity
            tarball
            srcrootdir
            pkgid
            pkgTextOverride
          moveTarballShippedDistDirectory
            verbosity
            distDirLayout
            srcrootdir
            pkgid
            dparams
        buildPkg srcdir builddir

unpackPackageTarball ::
  Verbosity ->
  FilePath ->
  FilePath ->
  PackageId ->
  Maybe CabalFileText ->
  IO ()
unpackPackageTarball verbosity tarball parentdir pkgid pkgTextOverride = do
  -- Unpack the tarball
  Utils.info verbosity $ "Extracting " ++ tarball ++ " to " ++ parentdir ++ "..."
  Tar.extractTarGzFile parentdir pkgsubdir tarball

  -- Overwrite the .cabal with the one from the index, when appropriate
  case pkgTextOverride of
    Nothing -> return ()
    Just pkgtxt -> do
      Utils.info verbosity $
        "Updating "
          ++ prettyShow pkgname <.> "cabal"
          ++ " with the latest revision from the index."
      Utils.writeFileAtomic cabalFile pkgtxt
  where
    cabalFile :: FilePath
    cabalFile =
      parentdir
        </> pkgsubdir
        </> prettyShow pkgname <.> "cabal"
    pkgsubdir = prettyShow pkgid
    pkgname = packageName pkgid

-- | This is a bit of a hacky workaround. A number of packages ship
-- pre-processed .hs files in a dist directory inside the tarball. We don't
-- use the standard 'dist' location so unless we move this dist dir to the
-- right place then we'll miss the shipped pre-processed files. This hacky
-- approach to shipped pre-processed files ought to be replaced by a proper
-- system, though we'll still need to keep this hack for older packages.
moveTarballShippedDistDirectory ::
  Verbosity ->
  DistDirLayout ->
  FilePath ->
  PackageId ->
  DistDirParams ->
  IO ()
moveTarballShippedDistDirectory
  verbosity
  DistDirLayout {distBuildDirectory}
  parentdir
  pkgid
  dparams = do
    distDirExists <- doesDirectoryExist tarballDistDir
    when distDirExists $ do
      Utils.debug verbosity $
        "Moving '"
          ++ tarballDistDir
          ++ "' to '"
          ++ targetDistDir
          ++ "'"
      -- TODO: [nice to have] or perhaps better to copy, and use a file monitor
      renameDirectory tarballDistDir targetDistDir
    where
      tarballDistDir = parentdir </> prettyShow pkgid </> "dist"
      targetDistDir = distBuildDirectory dparams
