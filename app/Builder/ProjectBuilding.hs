{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Builder.ProjectBuilding where

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS.Char8
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Traversable (for)
import Distribution.Client.DistDirLayout
import Distribution.Client.PackageHash
import Distribution.Client.ProjectBuilding.Types (BuildResult (..))
import Distribution.Client.ProjectPlanning hiding (setupHsBuildFlags, setupHsConfigureArgs, setupHsConfigureFlags, setupHsCopyFlags, setupHsHaddockFlags, setupHsRegisterFlags)
import Distribution.Client.ProjectPlanning.Types as Ty
import Distribution.Client.Setup hiding (buildCommand, cabalVersion, configureCommand, haddockCommand, packageName)
import Distribution.Client.Store
import Distribution.Client.Types (TestsResult (..))
import Distribution.Client.Types.BuildResults (DocsResult (..))
import Distribution.Client.Types.ConfiguredId (ConfiguredId (..))
import Distribution.Compat.Directory (listDirectory)
import Distribution.Compat.Lens ((^.))
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.InstalledPackageInfo qualified as Installed
import Distribution.Package
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Command (CommandUI (..), commandShowOptions)
import Distribution.Simple.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.InstallDirs qualified as InstallDirs
import Distribution.Simple.Program
import Distribution.Simple.Setup qualified as Cabal
import Distribution.Simple.Utils
import Distribution.Solver.Types.OptionalStanza
import Distribution.Types.ComponentName
import Distribution.Types.GivenComponent (GivenComponent (..))
import Distribution.Types.LibraryName
import Distribution.Types.PackageDescription.Lens (componentModules)
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.NubList
import Distribution.Verbosity (Verbosity)
import Distribution.Version
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath

buildAndInstallUnpackedPackage ::
  Verbosity ->
  StoreDirLayout ->
  ElaboratedSharedConfig ->
  ElaboratedConfiguredPackage ->
  FilePath ->
  Maybe FilePath ->
  IO BuildResult
buildAndInstallUnpackedPackage
  verbosity
  storeDirLayout
  pkgshared@ElaboratedSharedConfig {pkgConfigCompiler = compiler}
  pkg
  builddir
  mlogFile = do
    -- Configure phase
    setup' configureCommand configureFlags configureArgs

    -- Build phase
    setup buildCommand buildFlags

    -- Haddock phase
    whenHaddock $ setup haddockCommand haddockFlags

    -- Install phase
    let copyPkgFiles tmpDir = do
          let tmpDirNormalised = normalise tmpDir
          setup Cabal.copyCommand (copyFlags tmpDirNormalised)

          -- Note that the copy command has put the files into
          -- @$tmpDir/$prefix@ so we need to return this dir so
          -- the store knows which dir will be the final store entry.
          let prefix =
                normalise $
                  dropDrive (InstallDirs.prefix (elabInstallDirs pkg))
              entryDir = tmpDirNormalised </> prefix

          -- if there weren't anything to build, it might be that directory is not created
          -- the @setup Cabal.copyCommand@ above might do nothing.
          -- https://github.com/haskell/cabal/issues/4130
          createDirectoryIfMissingVerbose verbosity True entryDir

          let hashFileName = entryDir </> "cabal-hash.txt"
              outPkgHashInputs = renderPackageHashInputs (packageHashInputs pkgshared pkg)

          info verbosity $
            "creating file with the inputs used to compute the package hash: " ++ hashFileName

          LBS.writeFile hashFileName outPkgHashInputs

          debug verbosity "Package hash inputs:"
          traverse_
            (debug verbosity . ("> " ++))
            (lines $ LBS.Char8.unpack outPkgHashInputs)

          -- Ensure that there are no files in `tmpDir`, that are
          -- not in `entryDir`. While this breaks the
          -- prefix-relocatable property of the libraries, it is
          -- necessary on macOS to stay under the load command limit
          -- of the macOS mach-o linker. See also
          -- @PackageHash.hashedInstalledPackageIdVeryShort@.
          --
          -- We also normalise paths to ensure that there are no
          -- different representations for the same path. Like / and
          -- \\ on windows under msys.
          otherFiles <-
            filter (not . isPrefixOf entryDir)
              <$> listFilesRecursive tmpDirNormalised
          -- Here's where we could keep track of the installed files
          -- ourselves if we wanted to by making a manifest of the
          -- files in the tmp dir.
          return (entryDir, otherFiles)
          where
            listFilesRecursive :: FilePath -> IO [FilePath]
            listFilesRecursive path = do
              files <- fmap (path </>) <$> listDirectory path
              allFiles <- for files $ \file -> do
                isDir <- doesDirectoryExist file
                if isDir
                  then listFilesRecursive file
                  else return [file]
              return (concat allFiles)

        registerPkg
          | not (elabRequiresRegistration pkg) =
              debug verbosity $ "registerPkg: elab does NOT require registration for " ++ prettyShow uid
          | otherwise =
              info verbosity $ "register " ++ prettyShow uid
    --              -- We register ourselves rather than via Setup.hs. We need to
    --              -- grab and modify the InstalledPackageInfo. We decide what
    --              -- the installed package id is, not the build system.
    --              ipkg0 <- generateInstalledPackageInfo
    --              let ipkg = ipkg0 {Installed.installedUnitId = uid}
    --              assert
    --                ( elabRegisterPackageDBStack pkg
    --                    == storePackageDBStack compid
    --                )
    --                (return ())
    --              criticalSection registerLock $
    --                Cabal.registerPackage
    --                  verbosity
    --                  compiler
    --                  progdb
    --                  (storePackageDBStack compid)
    --                  ipkg
    --                  Cabal.defaultRegisterOptions
    --                    { Cabal.registerMultiInstance = True,
    --                      Cabal.registerSuppressFilesCheck = True
    --                    }

    -- Actual installation
    newStoreEntry verbosity storeDirLayout compid uid copyPkgFiles registerPkg >>= print

    -- TODO: [nice to have] we currently rely on Setup.hs copy to do the right
    -- thing. Although we do copy into an image dir and do the move into the
    -- final location ourselves, perhaps we ought to do some sanity checks on
    -- the image dir first.

    -- TODO: [required eventually] note that for nix-style
    -- installations it is not necessary to do the
    -- 'withWin32SelfUpgrade' dance, but it would be necessary for a
    -- shared bin dir.

    return
      BuildResult
        { buildResultDocs = DocsNotTried,
          buildResultTests = TestsNotTried,
          buildResultLogFile = mlogFile
        }
    where
      uid = installedUnitId pkg

      compid = compilerId compiler

      whenHaddock action
        | hasValidHaddockTargets pkg = action
        | otherwise = return ()

      configureCommand = Cabal.configureCommand defaultProgramDb

      configureFlags =
        setupHsConfigureFlags
          pkg
          pkgshared
          verbosity
          builddir

      configureArgs = setupHsConfigureArgs pkg

      buildCommand = Cabal.buildCommand defaultProgramDb

      buildFlags =
        setupHsBuildFlags
          pkg
          pkgshared
          verbosity
          builddir

      haddockCommand = Cabal.haddockCommand

      haddockFlags =
        setupHsHaddockFlags
          pkg
          pkgshared
          verbosity
          builddir

      -- generateInstalledPackageInfo :: IO InstalledPackageInfo
      -- generateInstalledPackageInfo =
      --  withTempInstalledPackageInfoFile
      --    verbosity
      --    distTempDirectory
      --    $ \pkgConfDest -> do
      --      let registerFlags =
      --            setupHsRegisterFlags
      --              pkg
      --              pkgshared
      --              verbosity
      --              builddir
      --              pkgConfDest
      --      setup Cabal.registerCommand registerFlags

      copyFlags destdir =
        setupHsCopyFlags
          pkg
          pkgshared
          verbosity
          builddir
          destdir

      setup :: CommandUI flags -> flags -> IO ()
      setup cmd flags =
        setup' cmd flags []

      setup' ::
        CommandUI flags ->
        flags ->
        [String] ->
        IO ()
      setup' cmd flags args =
        print $ unwords $ commandName cmd : commandShowOptions cmd flags ++ args

hasValidHaddockTargets :: ElaboratedConfiguredPackage -> Bool
hasValidHaddockTargets
  ElaboratedConfiguredPackage
    { elabPkgDescription,
      elabHaddockForeignLibs,
      elabHaddockExecutables,
      elabHaddockTestSuites,
      elabHaddockBenchmarks,
      elabHaddockInternal,
      elabBuildTargets,
      elabTestTargets,
      elabBenchTargets,
      elabReplTarget,
      elabHaddockTargets,
      elabBuildHaddocks
    }
    | not elabBuildHaddocks = False
    | otherwise = any componentHasHaddocks components
    where
      components :: [ComponentTarget]
      components =
        elabBuildTargets
          ++ elabTestTargets
          ++ elabBenchTargets
          ++ maybeToList elabReplTarget
          ++ elabHaddockTargets

      componentHasHaddocks :: ComponentTarget -> Bool
      componentHasHaddocks (ComponentTarget name _) =
        case name of
          CLibName LMainLibName -> hasHaddocks
          CLibName (LSubLibName _) -> elabHaddockInternal && hasHaddocks
          CFLibName _ -> elabHaddockForeignLibs && hasHaddocks
          CExeName _ -> elabHaddockExecutables && hasHaddocks
          CTestName _ -> elabHaddockTestSuites && hasHaddocks
          CBenchName _ -> elabHaddockBenchmarks && hasHaddocks
        where
          hasHaddocks = not (null (elabPkgDescription ^. componentModules name))

withTempInstalledPackageInfoFile ::
  Verbosity ->
  FilePath ->
  (FilePath -> IO ()) ->
  IO InstalledPackageInfo
withTempInstalledPackageInfoFile verbosity tempdir action =
  withTempDirectory verbosity tempdir "package-registration-" $ \dir -> do
    -- make absolute since @action@ will often change directory
    absDir <- canonicalizePath dir

    let pkgConfDest = absDir </> "pkgConf"
    action pkgConfDest

    readPkgConf "." pkgConfDest
  where
    readPkgConf :: FilePath -> FilePath -> IO InstalledPackageInfo
    readPkgConf pkgConfDir pkgConfFile = do
      pkgConfStr <- BS.readFile (pkgConfDir </> pkgConfFile)
      (warns, ipkg) <- case Installed.parseInstalledPackageInfo pkgConfStr of
        Left perrors -> die' verbosity $ "Couldn't parse the output of 'setup register --gen-pkg-config':" ++ show (unlines $ NE.toList perrors)
        Right (warns, ipkg) -> return (warns, ipkg)

      unless (null warns) $
        warn verbosity $
          unlines warns

      return ipkg

setupHsConfigureFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.ConfigFlags
setupHsConfigureFlags
  elab@ElaboratedConfiguredPackage
    { elabComponentId,
      elabInstantiatedWith,
      elabFlagAssignment,
      elabBuildPackageDBStack,
      elabVanillaLib,
      elabSharedLib,
      elabStaticLib,
      elabDynExe,
      elabFullyStaticExe,
      elabGHCiLib,
      elabProfLib,
      elabProfExe,
      elabProfLibDetail,
      elabProfExeDetail,
      elabCoverage,
      elabOptimization,
      elabSplitObjs,
      elabSplitSections,
      elabStripLibs,
      elabStripExes,
      elabDebugInfo,
      elabDumpBuildInfo,
      elabProgramPaths,
      elabProgramArgs,
      elabProgramPathExtra,
      elabConfigureScriptArgs,
      elabExtraLibDirs,
      elabExtraLibDirsStatic,
      elabExtraFrameworkDirs,
      elabExtraIncludeDirs,
      elabProgPrefix,
      elabProgSuffix,
      elabInstallDirs,
      elabPkgOrComp
    }
  ElaboratedSharedConfig {pkgConfigCompiler}
  verbosity
  builddir =
    ConfigFlags
      { configArgs,
        configPrograms_,
        configProgramPaths,
        configProgramArgs,
        configProgramPathExtra,
        configHcFlavor,
        configHcPath,
        configHcPkg,
        configVanillaLib,
        configProfLib,
        configSharedLib,
        configStaticLib,
        configDynExe,
        configFullyStaticExe,
        configProfExe,
        configProf,
        configProfDetail,
        configProfLibDetail,
        configConfigureArgs,
        configOptimization,
        configProgPrefix,
        configProgSuffix,
        configInstallDirs,
        configScratchDir,
        configExtraLibDirs,
        configExtraLibDirsStatic,
        configExtraFrameworkDirs,
        configExtraIncludeDirs,
        configIPID,
        configCID,
        configDeterministic,
        configDistPref,
        configCabalFilePath,
        configVerbosity,
        configUserInstall,
        configPackageDBs,
        configGHCiLib,
        configSplitSections,
        configSplitObjs,
        configStripExes,
        configStripLibs,
        configConstraints,
        configDependencies,
        configInstantiateWith,
        configConfigurationsFlags,
        configTests,
        configBenchmarks,
        configCoverage,
        configLibCoverage,
        configExactConfiguration,
        configFlagError,
        configRelocatable,
        configDebugInfo,
        configDumpBuildInfo,
        configUseResponseFiles,
        configAllowDependingOnPrivateLibs
      }
    where
      configArgs = mempty -- unused, passed via args
      configDistPref = toFlag builddir

      configCabalFilePath = mempty

      configVerbosity = toFlag verbosity

      configInstantiateWith = Map.toList elabInstantiatedWith

      configDeterministic = mempty -- doesn't matter, configIPID/configCID overridese
      configIPID = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (prettyShow (pkgInstalledId pkg))
        ElabComponent _ -> mempty

      configCID = case elabPkgOrComp of
        ElabPackage _ -> mempty
        ElabComponent _ -> toFlag elabComponentId

      configProgramPaths = Map.toList elabProgramPaths

      configProgramArgs = Map.toList $ Map.insertWith (++) "ghc" ["-hide-all-packages"] elabProgramArgs

      configProgramPathExtra = toNubList elabProgramPathExtra

      configHcFlavor = toFlag (compilerFlavor pkgConfigCompiler)

      configHcPath = mempty -- we use configProgramPaths instead
      configHcPkg = mempty -- we use configProgramPaths instead
      configVanillaLib = toFlag elabVanillaLib

      configSharedLib = toFlag elabSharedLib

      configStaticLib = toFlag elabStaticLib

      configDynExe = toFlag elabDynExe

      configFullyStaticExe = toFlag elabFullyStaticExe

      configGHCiLib = toFlag elabGHCiLib

      configProfExe = mempty

      configProfLib = toFlag elabProfLib

      configProf = toFlag elabProfExe

      -- configProfDetail is for exe+lib, but overridden by configProfLibDetail
      -- so we specify both so we can specify independently
      configProfDetail = toFlag elabProfExeDetail

      configProfLibDetail = toFlag elabProfLibDetail

      configCoverage = toFlag elabCoverage

      configLibCoverage = mempty

      configOptimization = toFlag elabOptimization

      configSplitSections = toFlag elabSplitSections

      configSplitObjs = toFlag elabSplitObjs

      configStripExes = toFlag elabStripExes

      configStripLibs = toFlag elabStripLibs

      configDebugInfo = toFlag elabDebugInfo

      configDumpBuildInfo = toFlag elabDumpBuildInfo

      configConfigurationsFlags = elabFlagAssignment

      configConfigureArgs = elabConfigureScriptArgs

      configExtraLibDirs = elabExtraLibDirs

      configExtraLibDirsStatic = elabExtraLibDirsStatic

      configExtraFrameworkDirs = elabExtraFrameworkDirs

      configExtraIncludeDirs = elabExtraIncludeDirs

      configProgPrefix = maybe mempty toFlag elabProgPrefix

      configProgSuffix = maybe mempty toFlag elabProgSuffix

      configInstallDirs =
        fmap
          (toFlag . InstallDirs.toPathTemplate)
          elabInstallDirs

      -- we only use configDependencies, unless we're talking to an old Cabal
      -- in which case we use configConstraints
      -- NB: This does NOT use InstallPlan.depends, which includes executable
      -- dependencies which should NOT be fed in here (also you don't have
      -- enough info anyway)
      configDependencies =
        [ GivenComponent
            (packageName srcid)
            ln
            cid
          | ConfiguredId srcid mb_cn cid <- elabLibDependencies elab,
            let ln = case mb_cn of
                  Just (CLibName lname) -> lname
                  Just _ -> error "non-library dependency"
                  Nothing -> LMainLibName
        ]

      configConstraints =
        case elabPkgOrComp of
          ElabPackage _ ->
            [ thisPackageVersionConstraint srcid
              | ConfiguredId srcid _ _uid <- elabLibDependencies elab
            ]
          ElabComponent _ -> []

      -- explicitly clear, then our package db stack
      -- TODO: [required eventually] have to do this differently for older Cabal versions
      configPackageDBs = Nothing : map Just elabBuildPackageDBStack

      configTests = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (TestStanzas `optStanzaSetMember` pkgStanzasEnabled pkg)
        ElabComponent _ -> mempty

      configBenchmarks = case elabPkgOrComp of
        ElabPackage pkg -> toFlag (BenchStanzas `optStanzaSetMember` pkgStanzasEnabled pkg)
        ElabComponent _ -> mempty

      configExactConfiguration = toFlag True

      configFlagError = mempty -- TODO: [research required] appears not to be implemented
      configRelocatable = mempty -- TODO: [research required] ???
      configScratchDir = mempty -- never use
      configUserInstall = mempty -- don't rely on defaults
      configPrograms_ = mempty -- never use, shouldn't exist
      configUseResponseFiles = mempty

      configAllowDependingOnPrivateLibs = Flag $ not $ libraryVisibilitySupported pkgConfigCompiler

setupHsConfigureArgs ::
  ElaboratedConfiguredPackage ->
  [String]
setupHsConfigureArgs (ElaboratedConfiguredPackage {elabPkgOrComp = ElabPackage _}) = []
setupHsConfigureArgs elab@(ElaboratedConfiguredPackage {elabPkgOrComp = ElabComponent comp}) =
  [showComponentTarget (packageId elab) (ComponentTarget cname WholeComponent)]
  where
    cname =
      fromMaybe
        (error "setupHsConfigureArgs: trying to configure setup")
        (compComponentName comp)

setupHsBuildFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.BuildFlags
setupHsBuildFlags _ _ verbosity builddir =
  Cabal.BuildFlags
    { buildProgramPaths = mempty, -- unused, set at configure time
      buildProgramArgs = mempty, -- unused, set at configure time
      buildVerbosity = toFlag verbosity,
      buildDistPref = toFlag builddir,
      buildNumJobs = mempty, -- TODO: [nice to have] sometimes want to use toFlag (Just numBuildJobs),
      buildArgs = mempty, -- unused, passed via args not flags
      buildCabalFilePath = mempty
    }

setupHsBuildArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBuildArgs elab@(ElaboratedConfiguredPackage {elabPkgOrComp = ElabPackage _})
  -- Fix for #3335, don't pass build arguments if it's not supported
  | elabSetupScriptCliVersion elab >= mkVersion [1, 17] =
      map (showComponentTarget (packageId elab)) (elabBuildTargets elab)
  | otherwise =
      []
setupHsBuildArgs (ElaboratedConfiguredPackage {elabPkgOrComp = ElabComponent _}) =
  []

setupHsTestFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.TestFlags
setupHsTestFlags (ElaboratedConfiguredPackage {..}) _ verbosity builddir =
  Cabal.TestFlags
    { testDistPref = toFlag builddir,
      testVerbosity = toFlag verbosity,
      testMachineLog = maybe mempty toFlag elabTestMachineLog,
      testHumanLog = maybe mempty toFlag elabTestHumanLog,
      testShowDetails = maybe (Flag Cabal.Always) toFlag elabTestShowDetails,
      testKeepTix = toFlag elabTestKeepTix,
      testWrapper = maybe mempty toFlag elabTestWrapper,
      testFailWhenNoTestSuites = toFlag elabTestFailWhenNoTestSuites,
      testOptions = elabTestTestOptions
    }

setupHsTestArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsTestArgs elab =
  mapMaybe (showTestComponentTarget (packageId elab)) (elabTestTargets elab)

setupHsBenchFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.BenchmarkFlags
setupHsBenchFlags (ElaboratedConfiguredPackage {..}) _ verbosity builddir =
  Cabal.BenchmarkFlags
    { benchmarkDistPref = toFlag builddir,
      benchmarkVerbosity = toFlag verbosity,
      benchmarkOptions = elabBenchmarkOptions
    }

setupHsBenchArgs :: ElaboratedConfiguredPackage -> [String]
setupHsBenchArgs elab =
  mapMaybe (showBenchComponentTarget (packageId elab)) (elabBenchTargets elab)

setupHsReplFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.ReplFlags
setupHsReplFlags _ sharedConfig verbosity builddir =
  Cabal.ReplFlags
    { replProgramPaths = mempty, -- unused, set at configure time
      replProgramArgs = mempty, -- unused, set at configure time
      replVerbosity = toFlag verbosity,
      replDistPref = toFlag builddir,
      replReload = mempty, -- only used as callback from repl
      replReplOptions = pkgConfigReplOptions sharedConfig -- runtime override for repl flags
    }

setupHsReplArgs :: ElaboratedConfiguredPackage -> [String]
setupHsReplArgs elab =
  maybe [] (\t -> [showComponentTarget (packageId elab) t]) (elabReplTarget elab)

-- TODO: should be able to give multiple modules in one component

setupHsCopyFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  FilePath ->
  Cabal.CopyFlags
setupHsCopyFlags _ _ verbosity builddir destdir =
  Cabal.CopyFlags
    { copyArgs = [], -- TODO: could use this to only copy what we enabled
      copyDest = toFlag (InstallDirs.CopyTo destdir),
      copyDistPref = toFlag builddir,
      copyVerbosity = toFlag verbosity,
      copyCabalFilePath = mempty
    }

setupHsRegisterFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  FilePath ->
  Cabal.RegisterFlags
setupHsRegisterFlags
  ElaboratedConfiguredPackage {..}
  _
  verbosity
  builddir
  pkgConfFile =
    Cabal.RegisterFlags
      { regPackageDB = mempty, -- misfeature
        regGenScript = mempty, -- never use
        regGenPkgConf = toFlag (Just pkgConfFile),
        regInPlace = case elabBuildStyle of
          BuildInplaceOnly -> toFlag True
          _ -> toFlag False,
        regPrintId = mempty, -- never use
        regDistPref = toFlag builddir,
        regArgs = [],
        regVerbosity = toFlag verbosity,
        regCabalFilePath = mempty
      }

setupHsHaddockFlags ::
  ElaboratedConfiguredPackage ->
  ElaboratedSharedConfig ->
  Verbosity ->
  FilePath ->
  Cabal.HaddockFlags
setupHsHaddockFlags (ElaboratedConfiguredPackage {..}) _ verbosity builddir =
  Cabal.HaddockFlags
    { haddockProgramPaths = mempty, -- unused, set at configure time
      haddockProgramArgs = mempty, -- unused, set at configure time
      haddockHoogle = toFlag elabHaddockHoogle,
      haddockHtml = toFlag elabHaddockHtml,
      haddockHtmlLocation = maybe mempty toFlag elabHaddockHtmlLocation,
      haddockForHackage = toFlag elabHaddockForHackage,
      haddockForeignLibs = toFlag elabHaddockForeignLibs,
      haddockExecutables = toFlag elabHaddockExecutables,
      haddockTestSuites = toFlag elabHaddockTestSuites,
      haddockBenchmarks = toFlag elabHaddockBenchmarks,
      haddockInternal = toFlag elabHaddockInternal,
      haddockCss = maybe mempty toFlag elabHaddockCss,
      haddockLinkedSource = toFlag elabHaddockLinkedSource,
      haddockQuickJump = toFlag elabHaddockQuickJump,
      haddockHscolourCss = maybe mempty toFlag elabHaddockHscolourCss,
      haddockContents = maybe mempty toFlag elabHaddockContents,
      haddockDistPref = toFlag builddir,
      haddockKeepTempFiles = mempty, -- TODO: from build settings
      haddockVerbosity = toFlag verbosity,
      haddockCabalFilePath = mempty,
      haddockArgs = mempty
    }

setupHsHaddockArgs :: ElaboratedConfiguredPackage -> [String]
-- TODO: Does the issue #3335 affects test as well
setupHsHaddockArgs elab =
  map (showComponentTarget (packageId elab)) (elabHaddockTargets elab)
