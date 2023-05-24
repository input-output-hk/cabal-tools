{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}

import Control.Monad (unless, when)
import Control.Monad.Writer.CPS (MonadWriter (..), execWriter)
import Data.Foldable
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Dependency (removeLowerBounds, removeUpperBounds)
import Distribution.Client.Dependency hiding (removeLowerBounds, removeUpperBounds)
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.IndexUtils qualified as IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Targets (UserConstraint, userToPackageConstraint)
import Distribution.Client.Types (UnresolvedSourcePackage, pkgSpecifierTarget)
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.PackageSpecifier (pkgSpecifierConstraints)
import Distribution.Client.Types.SourcePackageDb
import Distribution.Client.Utils (incVersion)
import Distribution.Compat.Graph (nodeKey)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..), installedComponentId, sourceComponentName)
import Distribution.Package (Package (packageId))
import Distribution.PackageDescription qualified as PD hiding (setupBuildInfo)
import Distribution.Pretty qualified as Cabal
import Distribution.Simple.Compiler
import Distribution.Simple.Flag qualified as Flag
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex qualified as Cabal.PackageIndex
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Modular.Assignment (Assignment, toCPs)
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Index qualified as Solver
import Distribution.Solver.Modular.IndexConversion
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Message (Message, showMessages)
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Modular.Solver (solve)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.InstalledPreference (InstalledPreference)
import Distribution.Solver.Types.InstalledPreference qualified as Preference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..))
import Distribution.Solver.Types.PackageConstraint (ConstraintScope (ScopeAnySetupQualifier), scopeToPackageName)
import Distribution.Solver.Types.PackageIndex qualified as Solver.PackageIndex
import Distribution.Solver.Types.PackagePreferences (PackagePreferences (..))
import Distribution.Solver.Types.PkgConfigDb (readPkgConfigDb)
import Distribution.Solver.Types.Settings (AvoidReinstalls (..), EnableBackjumping (..), ShadowPkgs (..), SolveExecutables (SolveExecutables), StrongFlags (StrongFlags))
import Distribution.Solver.Types.SourcePackage (SourcePackage (SourcePackage))
import Distribution.Solver.Types.SourcePackage qualified as SP
import Distribution.System (Platform (..), buildArch, buildOS)
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.Generic (ordNubBy)
import Distribution.Verbosity as Verbosity
import Distribution.Version
import Opts (parseOpts)
import PrettyPrintSimple (pPrint)
import SetupDeps (applyDefaultSetupDeps, cabalPkgname)
import SolverIndex (prettySolverFailure, printSolverIndex)

main :: IO ()
main = do
  let verbosity = Verbosity.normal
  (_prjRoot, distDirLayout) <- parseOpts

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      mempty

  let ProjectConfig {projectConfigShared, projectConfigBuildOnly} = projectConfig
      ProjectConfigShared {projectConfigPackageDBs} = projectConfigShared

  let corePackageDbs :: PackageDBStack
      corePackageDbs = applyPackageDbFlags [GlobalPackageDB] projectConfigPackageDBs

  (compiler, mPlatform, progDb) <- GHC.configure verbosity Nothing Nothing defaultProgramDb

  pkgConfigDb <- readPkgConfigDb verbosity progDb

  installedPkgIndex <-
    IndexUtils.getInstalledPackages
      verbosity
      compiler
      corePackageDbs
      progDb

  let installedPackages =
        foldl'
          (flip Cabal.PackageIndex.deleteSourcePackageId)
          installedPkgIndex
          [packageId pkg | SpecificSourcePackage pkg <- localPackages]

  when True $ do
    putStrLn "------------------------------------"
    putStrLn "----- installed packages index -----"
    putStrLn "------------------------------------"
    putStrLn ""
    for_ (Cabal.PackageIndex.allPackages installedPkgIndex) $ \ipi -> do
      putStrLn $ "unit-id: " ++ Cabal.prettyShow (installedUnitId ipi)
      putStrLn $ "installed-component-id: " ++ Cabal.prettyShow (installedComponentId ipi)
      putStrLn $ "source-package-id: " ++ Cabal.prettyShow (sourcePackageId ipi)
      putStrLn $ "component-name: " ++ Cabal.prettyShow (sourceComponentName ipi)
      putStrLn $ "visibility: " ++ Cabal.prettyShow (libVisibility ipi)
      putStrLn ""

  let SolverSettings
        { solverSettingConstraints,
          solverSettingPreferences,
          solverSettingFlagAssignment,
          solverSettingFlagAssignments,
          solverSettingAllowOlder,
          solverSettingAllowNewer,
          solverSettingMaxBackjumps,
          solverSettingReorderGoals,
          solverSettingCountConflicts,
          solverSettingFineGrainedConflicts,
          solverSettingMinimizeConflictSet,
          solverSettingStrongFlags,
          solverSettingAllowBootLibInstalls,
          solverSettingOnlyConstrained,
          solverSettingIndexState,
          solverSettingActiveRepos,
          solverSettingIndependentGoals,
          solverSettingPreferOldest
        } = resolveSolverSettings projectConfig

  (sourcePkgDb, totalIndexState, activeRepos) <-
    projectConfigWithSolverRepoContext verbosity projectConfigShared projectConfigBuildOnly $ \repoctx ->
      getSourcePackagesAtIndexState
        verbosity
        repoctx
        solverSettingIndexState
        solverSettingActiveRepos

  putStrLn "------------------------"
  putStrLn "----- repositories -----"
  putStrLn "------------------------"
  putStrLn $ "index-state: " ++ Cabal.prettyShow totalIndexState
  putStrLn $ "active-repositories: " ++ Cabal.prettyShow activeRepos
  putStrLn ""

  let platform@(Platform arch os) = fromMaybe (Platform buildArch buildOS) mPlatform

      SourcePackageDb sourcePkgIndex sourcePkgPrefs = sourcePkgDb

      sourcePackages :: Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
      sourcePackages =
        removeLowerBounds solverSettingAllowOlder
          $ removeUpperBounds solverSettingAllowNewer
          $ fmap (applyDefaultSetupDeps compiler platform)
          -- add local packages to sourcePkgIndex
          $ ( \idx' ->
                foldl'
                  (flip Solver.PackageIndex.insert)
                  idx'
                  [pkg | SpecificSourcePackage pkg <- localPackages]
            )
          $ sourcePkgIndex

      pkgStanzasEnabled :: Map.Map PackageName (Map.Map OptionalStanza Bool)
      pkgStanzasEnabled =
        localPackagesEnabledStanzas projectConfig localPackages

      constraints :: [LabeledPackageConstraint]
      constraints =
        mkConstraints
          compiler
          localPackages
          pkgStanzasEnabled
          solverSettingConstraints
          solverSettingFlagAssignments
          solverSettingFlagAssignment

      preferences :: [PackagePreference]
      preferences =
        mkPreferences
          localPackages
          pkgStanzasEnabled
          sourcePkgPrefs
          solverSettingPreferences

      preferenceDefault :: PackagesPreferenceDefault
      preferenceDefault =
        -- TODO: [required eventually] decide if we need to prefer
        -- installed for global packages, or prefer latest even for
        -- global packages. Perhaps should be configurable but with a
        -- different name than "upgrade-dependencies".
        ( if Flag.asBool solverSettingPreferOldest
            then PreferAllOldest
            else PreferLatestForSelected
        )

      targets :: Set PackageName
      targets = Set.fromList $ map pkgSpecifierTarget localPackages

      -- Constraints have to be converted into a finite map indexed by PN.
      gcs :: Map.Map PackageName [LabeledPackageConstraint]
      gcs =
        Map.fromListWith
          (<>)
          [ (scopeToPackageName scope, [lpc])
            | lpc@(LabeledPackageConstraint (PackageConstraint scope _property) _source) <- constraints
          ]

      cinfo = compilerInfo compiler

      idx :: Solver.Index
      idx =
        convPIs
          os
          arch
          cinfo
          gcs
          (ShadowPkgs False)
          (StrongFlags False)
          (SolveExecutables True)
          installedPackages
          sourcePackages

  printSolverIndex idx

  let solverConfig =
        SolverConfig
          { strongFlags = solverSettingStrongFlags,
            solverVerbosity = verbosity,
            solveExecutables = SolveExecutables True,
            shadowPkgs = ShadowPkgs False,
            reorderGoals = solverSettingReorderGoals,
            pruneAfterFirstSuccess = PruneAfterFirstSuccess False,
            onlyConstrained = solverSettingOnlyConstrained,
            minimizeConflictSet = solverSettingMinimizeConflictSet,
            maxBackjumps = solverSettingMaxBackjumps,
            independentGoals = solverSettingIndependentGoals,
            goalOrder = Nothing,
            fineGrainedConflicts = solverSettingFineGrainedConflicts,
            enableBackjumping = EnableBackjumping True,
            countConflicts = solverSettingCountConflicts,
            avoidReinstalls = AvoidReinstalls False,
            allowBootLibInstalls = solverSettingAllowBootLibInstalls
          }

      packagePreferences :: PackageName -> PackagePreferences
      packagePreferences = interpretPackagesPreference targets preferenceDefault preferences

      progress :: Progress Message SolverFailure (Assignment, RevDepMap)
      progress = toProgress $ solve solverConfig cinfo idx pkgConfigDb packagePreferences gcs targets

  foldProgress
    (\step rest -> putStrLn step >> rest)
    (putStrLn . prettySolverFailure)
    ( \(a, rdm) -> do
        pPrint a
        pPrint rdm
        pPrint $ ordNubBy nodeKey $ map (convCP installedPackages sourcePackages) (toCPs a rdm)
    )
    $ showMessages progress

mkConstraints ::
  Package pkg =>
  -- | compiler
  Compiler ->
  -- | localPackages
  [PackageSpecifier pkg] ->
  -- | pkgStanzasEnabled
  Map.Map PackageName (Map.Map OptionalStanza Bool) ->
  -- | ssConstraints
  [(UserConstraint, ConstraintSource)] ->
  -- | ssFlagAssignments (?)
  Map.Map PackageName PD.FlagAssignment ->
  -- | ssFlagAssignment (?)
  PD.FlagAssignment ->
  -- | result
  [LabeledPackageConstraint]
mkConstraints compiler localPackages pkgStanzasEnabled ssConstraints ssFlagAssignments ssFlagAssignment =
  execWriter $ do
    tell
      [ LabeledPackageConstraint
          ( PackageConstraint
              (ScopeAnySetupQualifier cabalPkgname)
              (PackagePropertyVersion $ orLaterVersion $ setupMinCabalVersion compiler)
          )
          ConstraintSetupCabalMinVersion
      ]

    tell
      [ LabeledPackageConstraint
          ( PackageConstraint
              (ScopeAnySetupQualifier cabalPkgname)
              (PackagePropertyVersion $ earlierVersion setupMaxCabalVersion)
          )
          ConstraintSetupCabalMaxVersion
      ]

    -- version constraints from the config file or command line
    for_ ssConstraints $ \(pc, src) ->
      tell [LabeledPackageConstraint (userToPackageConstraint pc) src]

    -- enable stanza constraints where the user asked to enable
    for_ localPackages $ \pkg -> do
      let pkgname = pkgSpecifierTarget pkg
          stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnabled
          stanzas = [stanza | stanza <- [minBound .. maxBound], Map.lookup stanza stanzaM == Just True]
      unless (null stanzas) $
        tell
          [ LabeledPackageConstraint
              ( PackageConstraint
                  (scopeToplevel pkgname)
                  (PackagePropertyStanzas stanzas)
              )
              ConstraintSourceConfigFlagOrTarget
          ]

    -- TODO: [nice to have] should have checked at some point that the package in question actually has these flags.
    for_ (Map.toList ssFlagAssignments) $ \(pkgname, flags) ->
      tell
        [ LabeledPackageConstraint
            ( PackageConstraint
                (scopeToplevel pkgname)
                (PackagePropertyFlags flags)
            )
            ConstraintSourceConfigFlagOrTarget
        ]

    -- TODO: [nice to have] we have user-supplied flags for unspecified local packages (as well as specific per-package flags). For the former we just apply all these flags to all local targets which is silly. We should check if the flags are appropriate.
    for_ localPackages $ \pkg -> do
      let flags = ssFlagAssignment
      unless (PD.nullFlagAssignment flags) $ do
        let pkgname = pkgSpecifierTarget pkg
        tell
          [ LabeledPackageConstraint
              ( PackageConstraint
                  (scopeToplevel pkgname)
                  (PackagePropertyFlags flags)
              )
              ConstraintSourceConfigFlagOrTarget
          ]

    tell (foldMap pkgSpecifierConstraints localPackages)

mkPreferences ::
  Package pkg =>
  -- | localPackages
  [PackageSpecifier pkg] ->
  -- | pkgStanzasEnabled
  Map.Map PackageName (Map.Map OptionalStanza Bool) ->
  -- | sourcePkgPrefs
  Map.Map PackageName VersionRange ->
  -- | ssPreferences
  [PackageVersionConstraint] ->
  -- | result
  [PackagePreference]
mkPreferences localPackages pkgStanzasEnabled sourcePkgPrefs ssPreferences =
  -- preferences from the config file or command line
  [PackageVersionPreference name ver | PackageVersionConstraint name ver <- ssPreferences]
    ++
    -- enable stanza preference unilaterally, regardless if the user asked
    -- accordingly or expressed no preference, to help hint the solver

    -- enable stanza preference unilaterally, regardless if the user asked
    -- accordingly or expressed no preference, to help hint the solver
    [ PackageStanzasPreference pkgname stanzas
      | pkg <- localPackages,
        let pkgname = pkgSpecifierTarget pkg
            stanzaM = Map.findWithDefault Map.empty pkgname pkgStanzasEnabled
            stanzas =
              [ stanza | stanza <- [minBound .. maxBound], Map.lookup stanza stanzaM /= Just False
              ],
        not (null stanzas)
    ]
    ++ [ PackageVersionPreference name ver
         | (name, ver) <- Map.toList sourcePkgPrefs
       ]

-- | Give an interpretation to the global 'PackagesPreference' as
--  specific per-package 'PackageVersionPreference'.
interpretPackagesPreference ::
  Set PackageName ->
  PackagesPreferenceDefault ->
  [PackagePreference] ->
  (PackageName -> PackagePreferences)
interpretPackagesPreference selected defaultPref prefs =
  \pkgname ->
    PackagePreferences
      (versionPref pkgname)
      (installPref pkgname)
      (stanzasPref pkgname)
  where
    versionPref :: PackageName -> [VersionRange]
    versionPref pkgname =
      fromMaybe [anyVersion] (Map.lookup pkgname versionPrefs)

    versionPrefs :: Map.Map PackageName [VersionRange]
    versionPrefs =
      Map.fromListWith
        (++)
        [ (pkgname, [pref])
          | PackageVersionPreference pkgname pref <- prefs
        ]

    installPref :: PackageName -> InstalledPreference
    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)

    installPrefs :: Map.Map PackageName InstalledPreference
    installPrefs =
      Map.fromList
        [ (pkgname, pref)
          | PackageInstalledPreference pkgname pref <- prefs
        ]

    installPrefDefault :: PackageName -> InstalledPreference
    installPrefDefault = case defaultPref of
      PreferAllLatest -> const Preference.PreferLatest
      PreferAllOldest -> const Preference.PreferOldest
      PreferAllInstalled -> const Preference.PreferInstalled
      PreferLatestForSelected -> \pkgname ->
        -- When you say cabal install foo, what you really mean is, prefer the
        -- latest version of foo, but the installed version of everything else
        if pkgname `Set.member` selected
          then Preference.PreferLatest
          else Preference.PreferInstalled

    stanzasPref :: PackageName -> [OptionalStanza]
    stanzasPref pkgname =
      fromMaybe [] (Map.lookup pkgname stanzasPrefs)

    stanzasPrefs :: Map.Map PackageName [OptionalStanza]
    stanzasPrefs =
      Map.fromListWith
        (\a b -> nub (a ++ b))
        [ (pkgname, pref)
          | PackageStanzasPreference pkgname pref <- prefs
        ]

-- | Append the given package databases to an existing PackageDBStack.
-- A @Nothing@ entry will clear everything before it.
applyPackageDbFlags :: PackageDBStack -> [Maybe PackageDB] -> PackageDBStack
applyPackageDbFlags dbs' [] = dbs'
applyPackageDbFlags _ (Nothing : dbs) = applyPackageDbFlags [] dbs
applyPackageDbFlags dbs' (Just db : dbs) = applyPackageDbFlags (dbs' ++ [db]) dbs

-- While we can talk to older Cabal versions (we need to be able to
-- do so for custom Setup scripts that require older Cabal lib
-- versions), we have problems talking to some older versions that
-- don't support certain features.
--
-- For example, Cabal-1.16 and older do not know about build targets.
-- Even worse, 1.18 and older only supported the --constraint flag
-- with source package ids, not --dependency with installed package
-- ids. That is bad because we cannot reliably select the right
-- dependencies in the presence of multiple instances (i.e. the
-- store). See issue #3932. So we require Cabal 1.20 as a minimum.
--
-- Moreover, lib:Cabal generally only supports the interface of
-- current and past compilers; in fact recent lib:Cabal versions
-- will warn when they encounter a too new or unknown GHC compiler
-- version (c.f. #415). To avoid running into unsupported
-- configurations we encode the compatibility matrix as lower
-- bounds on lib:Cabal here (effectively corresponding to the
-- respective major Cabal version bundled with the respective GHC
-- release).
--
-- etc.
-- GHC 9.2   needs  Cabal >= 3.6
-- GHC 9.0   needs  Cabal >= 3.4
-- GHC 8.10  needs  Cabal >= 3.2
-- GHC 8.8   needs  Cabal >= 3.0
-- GHC 8.6   needs  Cabal >= 2.4
-- GHC 8.4   needs  Cabal >= 2.2
-- GHC 8.2   needs  Cabal >= 2.0
-- GHC 8.0   needs  Cabal >= 1.24
-- GHC 7.10  needs  Cabal >= 1.22
--
-- (NB: we don't need to consider older GHCs as Cabal >= 1.20 is
-- the absolute lower bound)
--
-- TODO: long-term, this compatibility matrix should be
--       stored as a field inside 'Distribution.Compiler.Compiler'
setupMinCabalVersion :: Compiler -> Version
setupMinCabalVersion compiler =
  if
      | isGHC, compVer >= mkVersion [9, 6] -> mkVersion [3, 10]
      | isGHC, compVer >= mkVersion [9, 4] -> mkVersion [3, 8]
      | isGHC, compVer >= mkVersion [9, 2] -> mkVersion [3, 6]
      | isGHC, compVer >= mkVersion [9, 0] -> mkVersion [3, 4]
      | isGHC, compVer >= mkVersion [8, 10] -> mkVersion [3, 2]
      | isGHC, compVer >= mkVersion [8, 8] -> mkVersion [3, 0]
      | isGHC, compVer >= mkVersion [8, 6] -> mkVersion [2, 4]
      | isGHC, compVer >= mkVersion [8, 4] -> mkVersion [2, 2]
      | isGHC, compVer >= mkVersion [8, 2] -> mkVersion [2, 0]
      | isGHC, compVer >= mkVersion [8, 0] -> mkVersion [1, 24]
      | isGHC, compVer >= mkVersion [7, 10] -> mkVersion [1, 22]
      | otherwise -> mkVersion [1, 20]
  where
    isGHC = compFlav `elem` [GHC, GHCJS]
    compFlav = compilerFlavor compiler
    compVer = compilerVersion compiler

-- As we can't predict the future, we also place a global upper
-- bound on the lib:Cabal version we know how to interact with:
--
-- The upper bound is computed by incrementing the current major
-- version twice in order to allow for the current version, as
-- well as the next adjacent major version (one of which will not
-- be released, as only "even major" versions of Cabal are
-- released to Hackage or bundled with proper GHC releases).
--
-- For instance, if the current version of cabal-install is an odd
-- development version, e.g.  Cabal-2.1.0.0, then we impose an
-- upper bound `setup.Cabal < 2.3`; if `cabal-install` is on a
-- stable/release even version, e.g. Cabal-2.2.1.0, the upper
-- bound is `setup.Cabal < 2.4`. This gives us enough flexibility
-- when dealing with development snapshots of Cabal and cabal-install.
--
setupMaxCabalVersion :: Version
setupMaxCabalVersion =
  alterVersion (take 2) $ incVersion 1 $ incVersion 1 cabalVersion

localPackagesEnabledStanzas ::
  ProjectConfig ->
  [PackageSpecifier (SourcePackage (PackageLocation loc))] ->
  Map.Map PD.PackageName (Map.Map OptionalStanza Bool)
localPackagesEnabledStanzas projectConfig localPackages =
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

            stanzas
              | shouldReallyBeLocal pkg =
                  Map.fromList $
                    [(TestStanzas, enabled) | enabled <- Flag.flagToList testsEnabled]
                      <> [(BenchStanzas, enabled) | enabled <- Flag.flagToList benchmarksEnabled]
              | otherwise =
                  Map.fromList [(TestStanzas, False), (BenchStanzas, False)]
    ]

shouldReallyBeLocal :: PackageSpecifier (SourcePackage (PackageLocation loc)) -> Bool
shouldReallyBeLocal (SpecificSourcePackage (SourcePackage {SP.srcpkgSource = LocalUnpackedPackage {}})) = True
shouldReallyBeLocal _otherwise = False
