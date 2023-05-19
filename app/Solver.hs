{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Monad (unless, when)
import Control.Monad.Writer.CPS (MonadWriter (..), execWriter)
import Data.Coerce (coerce)
import Data.Foldable
import Data.Functor.Foldable (Base, Recursive (..))
import Data.List (nub)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semialign qualified as Semialign
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Data.These (fromThese)
import Distribution.CabalSpecVersion (CabalSpecVersion, cabalSpecLatest, cabalSpecMinimumLibraryVersion)
import Distribution.Client.Dependency hiding (removeLowerBounds, removeUpperBounds)
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.IndexUtils qualified as IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types (SetupScriptStyle (..))
import Distribution.Client.Targets (userToPackageConstraint)
import Distribution.Client.Types (UnresolvedSourcePackage, pkgSpecifierTarget)
import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.PackageSpecifier (pkgSpecifierConstraints)
import Distribution.Client.Types.SourcePackageDb
import Distribution.Client.Utils (incVersion)
import Distribution.Compat.Graph (nodeKey)
import Distribution.Compat.Lens
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..), installedComponentId, sourceComponentName)
import Distribution.Package (Package (packageId), packageName)
import Distribution.PackageDescription qualified as PD hiding (setupBuildInfo)
import Distribution.PackageDescription.Configuration qualified as PD
import Distribution.Pretty qualified as Cabal
import Distribution.Simple.Compiler
import Distribution.Simple.Flag qualified as Flag
import Distribution.Simple.GHC qualified as GHC
import Distribution.Simple.PackageIndex qualified as Cabal.PackageIndex
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Solver.Modular (PruneAfterFirstSuccess (..), SolverConfig (..))
import Distribution.Solver.Modular.Assignment (toCPs)
import Distribution.Solver.Modular.ConfiguredConversion (convCP)
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index (ComponentInfo (ComponentInfo), IsBuildable (IsBuildable), IsVisible (IsVisible), PInfo (..))
import Distribution.Solver.Modular.IndexConversion
import Distribution.Solver.Modular.Log (SolverFailure (BackjumpLimitReached, ExhaustiveSearch))
import Distribution.Solver.Modular.Message (showMessages)
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.RetryLog (toProgress)
import Distribution.Solver.Modular.Solver (solve)
import Distribution.Solver.Types.ComponentDeps (Component)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.InstalledPreference (InstalledPreference)
import Distribution.Solver.Types.InstalledPreference qualified as Preference
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), showStanza)
import Distribution.Solver.Types.PackageConstraint (ConstraintScope (ScopeAnySetupQualifier), scopeToPackageName)
import Distribution.Solver.Types.PackageIndex qualified as Solver.PackageIndex
import Distribution.Solver.Types.PackagePreferences (PackagePreferences (..))
import Distribution.Solver.Types.PkgConfigDb (readPkgConfigDb)
import Distribution.Solver.Types.Settings (AvoidReinstalls (..), EnableBackjumping (..), ShadowPkgs (..), SolveExecutables (SolveExecutables), StrongFlags (StrongFlags))
import Distribution.Solver.Types.SourcePackage (SourcePackage (SourcePackage))
import Distribution.Solver.Types.SourcePackage qualified as SP
import Distribution.System (OS (Windows), Platform (..), buildArch, buildOS)
import Distribution.Types.Dependency (Dependency (Dependency), mainLibSet)
import Distribution.Types.GenericPackageDescription.Lens
import Distribution.Types.PackageDescription qualified as PD
import Distribution.Types.PackageDescription.Lens
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.Generic (ordNubBy)
import Distribution.Verbosity as Verbosity
import Distribution.Version
import GHC.Generics (Generic)
import Opts (parseOpts)
import PrettyPrint (pPrint)
import Prettyprinter
import SourcePackage.Lens

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

  let ProjectConfig {projectConfigShared} = projectConfig

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

  (compiler, mPlatform, progDb) <- GHC.configure verbosity Nothing Nothing defaultProgramDb

  let cinfo = compilerInfo compiler

  pkgConfigDb <- readPkgConfigDb verbosity progDb

  let corePackageDbs = applyPackageDbFlags [GlobalPackageDB] (projectConfigPackageDBs projectConfigShared)

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

  (sourcePkgDb, totalIndexState, activeRepos) <-
    projectConfigWithSolverRepoContext verbosity projectConfigShared (projectConfigBuildOnly projectConfig) $ \repoctx ->
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
  let SourcePackageDb sourcePkgIndex sourcePkgPrefs = sourcePkgDb

  let sourcePackages =
        removeLowerBounds solverSettingAllowOlder $
          removeUpperBounds solverSettingAllowNewer $
            fmap (applyDefaultSetupDeps compiler platform) $
              -- add local packages to sourcePkgIndex
              foldl'
                (flip Solver.PackageIndex.insert)
                sourcePkgIndex
                [pkg | SpecificSourcePackage pkg <- localPackages]

  let pkgStanzasEnabled = localPackagesEnabledStanzas projectConfig localPackages

  let constraints = execWriter $ do
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
        for_ solverSettingConstraints $ \(pc, src) ->
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
        for_ (Map.toList solverSettingFlagAssignments) $ \(pkgname, flags) ->
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
          let flags = solverSettingFlagAssignment
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

  let preferences =
        -- preferences from the config file or command line
        [PackageVersionPreference name ver | PackageVersionConstraint name ver <- solverSettingPreferences]
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

  let preferenceDefault =
        -- TODO: [required eventually] decide if we need to prefer
        -- installed for global packages, or prefer latest even for
        -- global packages. Perhaps should be configurable but with a
        -- different name than "upgrade-dependencies".
        ( if Flag.asBool solverSettingPreferOldest
            then PreferAllOldest
            else PreferLatestForSelected
        )

  let targets = Set.fromList $ map pkgSpecifierTarget localPackages

  -- Constraints have to be converted into a finite map indexed by PN.
  let gcs =
        Map.fromListWith
          (<>)
          [ (scopeToPackageName scope, [lpc])
            | lpc@(LabeledPackageConstraint (PackageConstraint scope _property) _source) <- constraints
          ]

  let idx =
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

  let packagePreferences = interpretPackagesPreference targets preferenceDefault preferences
  let progress = toProgress $ solve solverConfig cinfo idx pkgConfigDb packagePreferences gcs targets

  foldProgress
    (\step rest -> putStrLn step >> rest)
    (putStrLn . prettySolverFailure)
    ( \(a, rdm) -> do
        pPrint a
        pPrint rdm
        pPrint $ ordNubBy nodeKey $ map (convCP installedPackages sourcePackages) (toCPs a rdm)
    )
    $ showMessages progress

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
    versionPrefs =
      Map.fromListWith
        (++)
        [ (pkgname, [pref])
          | PackageVersionPreference pkgname pref <- prefs
        ]

    installPref :: PackageName -> InstalledPreference
    installPref pkgname =
      fromMaybe (installPrefDefault pkgname) (Map.lookup pkgname installPrefs)
    installPrefs =
      Map.fromList
        [ (pkgname, pref)
          | PackageInstalledPreference pkgname pref <- prefs
        ]
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
    stanzasPrefs =
      Map.fromListWith
        (\a b -> nub (a ++ b))
        [ (pkgname, pref)
          | PackageStanzasPreference pkgname pref <- prefs
        ]

applyDefaultSetupDeps :: Compiler -> Platform -> UnresolvedSourcePackage -> UnresolvedSourcePackage
applyDefaultSetupDeps comp platform srcpkg =
  over
    (srcpkgDescription . packageDescription . setupBuildInfo)
    ( \case
        Just sbi ->
          Just sbi
        Nothing ->
          case defaultSetupDeps comp platform (srcpkg ^. srcpkgDescription . packageDescription) of
            Nothing ->
              Nothing
            Just deps
              | isCustom -> Just PD.SetupBuildInfo {PD.defaultSetupDepends = True, PD.setupDepends = deps}
              | otherwise -> Nothing
    )
    srcpkg
  where
    isCustom = PD.buildType pkgdesc == PD.Custom
    gpkgdesc = view srcpkgDescription srcpkg
    pkgdesc = view packageDescription gpkgdesc

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

--
-- Mostly copied from Distribution.Client.Dependency because they are not exported
--

removeUpperBounds ::
  AllowNewer ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
removeUpperBounds (AllowNewer relDeps) = removeBounds RelaxUpper relDeps

-- | Dual of 'removeUpperBounds'
removeLowerBounds ::
  AllowOlder ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
removeLowerBounds (AllowOlder relDeps) = removeBounds RelaxLower relDeps

data RelaxKind = RelaxLower | RelaxUpper

-- | Common internal implementation of 'removeLowerBounds'/'removeUpperBounds'
removeBounds ::
  RelaxKind ->
  RelaxDeps ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage ->
  Solver.PackageIndex.PackageIndex UnresolvedSourcePackage
removeBounds relKind relDeps sourcePkgIndex
  | not (isRelaxDeps relDeps) = sourcePkgIndex
  | otherwise = fmap relaxDeps sourcePkgIndex
  where
    relaxDeps :: UnresolvedSourcePackage -> UnresolvedSourcePackage
    relaxDeps = over srcpkgDescription (relaxPackageDeps relKind relDeps)

-- | Relax the dependencies of this package if needed.
--
-- Helper function used by 'removeBounds'
relaxPackageDeps ::
  RelaxKind ->
  RelaxDeps ->
  PD.GenericPackageDescription ->
  PD.GenericPackageDescription
relaxPackageDeps relKind RelaxDepsAll gpd =
  PD.transformAllBuildDepends relaxAll gpd
  where
    relaxAll :: Dependency -> Dependency
    relaxAll (Dependency pkgName verRange cs) =
      Dependency pkgName (removeBound relKind RelaxDepModNone verRange) cs
relaxPackageDeps relKind (RelaxDepsSome depsToRelax0) gpd =
  PD.transformAllBuildDepends relaxSome gpd
  where
    thisPkgName = packageName gpd
    thisPkgId = packageId gpd

    thisPkgInScope :: RelaxDepScope -> Bool
    thisPkgInScope RelaxDepScopeAll = True
    thisPkgInScope (RelaxDepScopePackage p0) | p0 == thisPkgName = True
    thisPkgInScope (RelaxDepScopePackageId p0) | p0 == thisPkgId = True
    thisPkgInScope _otherwise = False

    depsToRelax :: Map.Map RelaxDepSubject RelaxDepMod
    depsToRelax = Map.fromList [(p, rdm) | (RelaxedDep depScope rdm p) <- depsToRelax0, thisPkgInScope depScope]

    relaxSome :: Dependency -> Dependency
    relaxSome d@(Dependency depName verRange cs)
      | Just relMod <- Map.lookup RelaxDepSubjectAll depsToRelax =
          -- a '*'-subject acts absorbing, for consistency with
          -- the 'Semigroup RelaxDeps' instance
          Dependency depName (removeBound relKind relMod verRange) cs
      | Just relMod <- Map.lookup (RelaxDepSubjectPkg depName) depsToRelax =
          Dependency depName (removeBound relKind relMod verRange) cs
      | otherwise = d -- no-op

-- | Internal helper for 'relaxPackageDeps'
removeBound :: RelaxKind -> RelaxDepMod -> VersionRange -> VersionRange
removeBound RelaxLower RelaxDepModNone = removeLowerBound
removeBound RelaxUpper RelaxDepModNone = removeUpperBound
removeBound RelaxLower RelaxDepModCaret = transformCaretLower
removeBound RelaxUpper RelaxDepModCaret = transformCaretUpper

--
-- Again not exported
--

-- | Part of our Setup.hs handling policy is implemented by getting the solver
-- to work out setup dependencies for packages. The solver already handles
-- packages that explicitly specify setup dependencies, but we can also tell
-- the solver to treat other packages as if they had setup dependencies.
-- That's what this function does, it gets called by the solver for all
-- packages that don't already have setup dependencies.
--
-- The dependencies we want to add is different for each 'SetupScriptStyle'.
--
-- Note that adding default deps means these deps are actually /added/ to the
-- packages that we get out of the solver in the 'SolverInstallPlan'. Making
-- implicit setup deps explicit is a problem in the post-solver stages because
-- we still need to distinguish the case of explicit and implicit setup deps.
-- See 'rememberImplicitSetupDeps'.
--
-- Note in addition to adding default setup deps, we also use
-- 'addSetupCabalMinVersionConstraint' (in 'planPackages') to require
-- @Cabal >= 1.20@ for Setup scripts.
defaultSetupDeps ::
  Compiler ->
  Platform ->
  PD.PackageDescription ->
  Maybe [Dependency]
defaultSetupDeps compiler platform pkg =
  case packageSetupScriptStyle pkg of
    -- For packages with build type custom that do not specify explicit
    -- setup dependencies, we add a dependency on Cabal and a number
    -- of other packages.
    SetupCustomImplicitDeps ->
      Just $
        [ Dependency depPkgname anyVersion mainLibSet
          | depPkgname <- legacyCustomSetupPkgs compiler platform
        ]
          ++ [ Dependency cabalPkgname cabalConstraint mainLibSet
               | packageName pkg /= cabalPkgname
             ]
      where
        -- The Cabal dep is slightly special:
        -- \* We omit the dep for the Cabal lib itself, since it bootstraps.
        -- \* We constrain it to be < 1.25
        --
        -- Note: we also add a global constraint to require Cabal >= 1.20
        -- for Setup scripts (see use addSetupCabalMinVersionConstraint).
        --
        cabalConstraint =
          orLaterVersion (csvToVersion (PD.specVersion pkg))
            `intersectVersionRanges` earlierVersion cabalCompatMaxVer
        -- The idea here is that at some point we will make significant
        -- breaking changes to the Cabal API that Setup.hs scripts use.
        -- So for old custom Setup scripts that do not specify explicit
        -- constraints, we constrain them to use a compatible Cabal version.
        cabalCompatMaxVer = mkVersion [1, 25]

    -- For other build types (like Simple) if we still need to compile an
    -- external Setup.hs, it'll be one of the simple ones that only depends
    -- on Cabal and base.
    SetupNonCustomExternalLib ->
      Just
        [ Dependency cabalPkgname cabalConstraint mainLibSet,
          Dependency basePkgname anyVersion mainLibSet
        ]
      where
        cabalConstraint = orLaterVersion (csvToVersion (PD.specVersion pkg))

    -- The internal setup wrapper method has no deps at all.
    SetupNonCustomInternalLib -> Just []
    -- This case gets ruled out by the caller, planPackages, see the note
    -- above in the SetupCustomImplicitDeps case.
    SetupCustomExplicitDeps ->
      error $
        "defaultSetupDeps: called for a package with explicit "
          ++ "setup deps: "
          ++ Cabal.prettyShow (packageId pkg)
  where
    -- we require one less
    --
    -- This maps e.g. CabalSpecV3_0 to mkVersion [2,5]
    csvToVersion :: CabalSpecVersion -> Version
    csvToVersion = mkVersion . cabalSpecMinimumLibraryVersion

---------------------------
-- Setup.hs script policy
--

-- Handling for Setup.hs scripts is a bit tricky, part of it lives in the
-- solver phase, and part in the elaboration phase. We keep the helper
-- functions for both phases together here so at least you can see all of it
-- in one place.
--
-- There are four major cases for Setup.hs handling:
--
--  1. @build-type@ Custom with a @custom-setup@ section
--  2. @build-type@ Custom without a @custom-setup@ section
--  3. @build-type@ not Custom with @cabal-version >  $our-cabal-version@
--  4. @build-type@ not Custom with @cabal-version <= $our-cabal-version@
--
-- It's also worth noting that packages specifying @cabal-version: >= 1.23@
-- or later that have @build-type@ Custom will always have a @custom-setup@
-- section. Therefore in case 2, the specified @cabal-version@ will always be
-- less than 1.23.
--
-- In cases 1 and 2 we obviously have to build an external Setup.hs script,
-- while in case 4 we can use the internal library API.
--
-- TODO:In case 3 we should fail. We don't know how to talk to
-- newer ./Setup.hs
--
-- data SetupScriptStyle = ...  -- see ProjectPlanning.Types

-- | Work out the 'SetupScriptStyle' given the package description.
packageSetupScriptStyle :: PD.PackageDescription -> SetupScriptStyle
packageSetupScriptStyle pkg
  | buildType == PD.Custom,
    Just setupbi <- PD.setupBuildInfo pkg, -- does have a custom-setup stanza
    not (PD.defaultSetupDepends setupbi) -- but not one we added internally
    =
      SetupCustomExplicitDeps
  | buildType == PD.Custom,
    Just setupbi <- PD.setupBuildInfo pkg, -- we get this case post-solver as
    PD.defaultSetupDepends setupbi -- the solver fills in the deps
    =
      SetupCustomImplicitDeps
  | buildType == PD.Custom,
    Nothing <- PD.setupBuildInfo pkg -- we get this case pre-solver
    =
      SetupCustomImplicitDeps
  -- here we should fail.
  | PD.specVersion pkg > cabalSpecLatest -- one cabal-install is built against
    =
      SetupNonCustomExternalLib
  | otherwise =
      SetupNonCustomInternalLib
  where
    buildType = PD.buildType pkg

legacyCustomSetupPkgs :: Compiler -> Platform -> [PackageName]
legacyCustomSetupPkgs compiler (Platform _ os) =
  map mkPackageName $
    [ "array",
      "base",
      "binary",
      "bytestring",
      "containers",
      "deepseq",
      "directory",
      "filepath",
      "pretty",
      "process",
      "time",
      "transformers"
    ]
      ++ ["Win32" | os == Windows]
      ++ ["unix" | os /= Windows]
      ++ ["ghc-prim" | isGHC]
      ++ ["template-haskell" | isGHC]
      ++ ["old-time" | notGHC710]
  where
    isGHC = compilerCompatFlavor GHC compiler
    notGHC710 = case compilerCompatVersion GHC compiler of
      Nothing -> False
      Just v -> v <= mkVersion [7, 9]

basePkgname :: PD.PackageName
basePkgname = mkPackageName "base"

cabalPkgname :: PD.PackageName
cabalPkgname = mkPackageName "cabal"

--
--

newtype My a = My a

myPretty :: Pretty (My a) => a -> Doc ann
myPretty = pretty . My

instance Cabal.Pretty qpn => Prettyprinter.Pretty (My (FlaggedDep qpn)) where
  pretty (My (Simple (LDep _dependencyReason dep) component)) =
    fromString (Cabal.prettyShow component) <+> "depends on" <+> myPretty dep
  pretty (My (Flagged (FN pn flag) _finfo trueDeps falseDeps))
    | null trueDeps && null falseDeps =
        mempty
    | null falseDeps =
        "when"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 (vsep (map myPretty trueDeps))
    | null trueDeps =
        "unless"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 (vsep (map myPretty falseDeps))
    | otherwise =
        "if"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 ("then" <+> align (vsep (map myPretty trueDeps)))
            <> line
            <> indent 2 ("else" <+> align (vsep (map myPretty falseDeps)))
  pretty (My (Stanza (SN _pn stanza) trueDeps))
    | null trueDeps =
        mempty
    | otherwise =
        "when"
          <+> fromString (showStanza stanza)
          <+> "is enabled"
            <> line
            <> indent 2 (vsep (map myPretty trueDeps))

-- prettyFlaggedDeps :: Cabal.Pretty qpn => FlaggedDeps qpn -> Doc ann
-- prettyFlaggedDeps deps =
--   align (vsep [myPretty dep | dep <- deps, not (isNull dep)])
--   where
--     isNull (Flagged _fn _fInfo trueDeps falseDeps) | null trueDeps && null falseDeps = True
--     isNull (Stanza _sn trueDeps) | null trueDeps = True
--     isNull _otherwise = False

instance Cabal.Pretty qpn => Pretty (My (Dep qpn)) where
  pretty (My (Dep pkgC ci)) =
    fromString (prettyPkgC pkgC) <+> prettyCI ci
  pretty (My (Ext extension)) =
    "extension" <+> fromString (show extension)
  pretty (My (Lang language)) =
    "language" <+> fromString (show language)
  pretty (My (Pkg pkgConfigName pkgConfigVR)) =
    fromString (Cabal.prettyShow pkgConfigName) <+> fromString (Cabal.prettyShow pkgConfigVR)

prettySolverFailure :: SolverFailure -> String
prettySolverFailure BackjumpLimitReached = "BackjumpLimitReached!"
prettySolverFailure (ExhaustiveSearch cs cm) = "ExhaustiveSearch! conflict set: " ++ show cs ++ " conflict map: " ++ show cm

prettyCI :: CI -> Doc ann
prettyCI (Fixed i) = "fixed" <+> fromString (showI i)
prettyCI (Constrained vr) | isAnyVersion vr = ""
prettyCI (Constrained vr) | otherwise = fromString (Cabal.prettyShow vr)

prettyEC :: IsString a => ExposedComponent -> a
prettyEC (ExposedLib libName) = fromString $ Cabal.prettyShow $ PD.CLibName libName
prettyEC (ExposedExe exeName) = fromString $ Cabal.prettyShow $ PD.CExeName exeName

prettyPkgC :: Cabal.Pretty qpn => PkgComponent qpn -> String
prettyPkgC (PkgComponent pn ec) = Cabal.prettyShow pn <> ":" <> prettyEC ec

prettyExposedComponentInfo :: ExposedComponent -> ComponentInfo -> Doc ann
prettyExposedComponentInfo ec ci = prettyEC ec <+> "(" <> prettyCompInfo ci <> ")"
  where
    prettyCompInfo (ComponentInfo visible buildable) = hsep [prettyIV visible, prettyIB buildable]

    prettyIV (IsVisible True) = "visible"
    prettyIV (IsVisible False) = mempty

    prettyIB (IsBuildable True) = "buildable"
    prettyIB (IsBuildable False) = mempty

deriving instance Show qpn => Show (FlaggedDep qpn)

deriving instance Show qpn => Show (Dep qpn)

deriving instance Show qpn => Show (LDep qpn)

newtype RecursiveFlaggedDep qpn = RecursiveFlaggedDep (FlaggedDep qpn)

type instance Base (RecursiveFlaggedDep qpn) = RecursiveFlaggedDepF qpn

data RecursiveFlaggedDepF qpn f
  = RecursiveFlaggedF (FN qpn) FInfo [f] [f]
  | RecursiveSimpleF (LDep qpn) Component
  | RecursiveStanzaF (SN qpn) [f]
  deriving (Functor, Generic)

instance Recursive (RecursiveFlaggedDep qpn) where
  project (RecursiveFlaggedDep (Flagged fn fInfo trueDeps falseDeps)) =
    RecursiveFlaggedF fn fInfo (coerce trueDeps) (coerce falseDeps)
  project (RecursiveFlaggedDep (Simple lDep comp)) =
    RecursiveSimpleF lDep comp
  project (RecursiveFlaggedDep (Stanza sn trueDeps)) =
    RecursiveStanzaF sn (coerce trueDeps)

type ComponentFlaggedDeps qpn = [ComponentFlaggedDep qpn]

data ComponentFlaggedDep qpn
  = ComponentSimple
      (LDep qpn)
  | ComponentFlagged
      (FN qpn)
      FInfo
      [ComponentFlaggedDep qpn]
      -- ^ trueDeps
      [ComponentFlaggedDep qpn]
      -- ^ falseDeps
  | ComponentStanza
      (SN qpn)
      [ComponentFlaggedDep qpn]
  deriving (Show)

toComponents :: FlaggedDeps qpn -> Map Component (ComponentFlaggedDeps qpn)
toComponents = coerce . foldMap (cata go . RecursiveFlaggedDep)
  where
    go (RecursiveFlaggedF fn fInfo trueDeps falseDeps) =
      Semialign.alignWith
        (\theseDeps -> [uncurry (ComponentFlagged fn fInfo) $ fromThese [] [] theseDeps])
        (mconcat trueDeps)
        (mconcat falseDeps)
    go (RecursiveSimpleF lDep comp) =
      MonoidalMap.singleton comp [ComponentSimple lDep]
    go (RecursiveStanzaF sn trueDeps) =
      fmap (\deps -> [ComponentStanza sn deps]) $ mconcat trueDeps

instance Cabal.Pretty qpn => Pretty (ComponentFlaggedDep qpn) where
  pretty (ComponentSimple (LDep _dependencyReason dep)) =
    myPretty dep
  pretty (ComponentFlagged (FN pn flag) _finfo trueDeps falseDeps)
    | null trueDeps && null falseDeps =
        mempty
    | null falseDeps =
        "when"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 (vsep (map pretty trueDeps))
    | null trueDeps =
        "unless"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 (vsep (map pretty falseDeps))
    | otherwise =
        "if"
          <+> fromString (Cabal.prettyShow pn <> ":" <> Cabal.prettyShow flag)
            <> line
            <> indent 2 ("then" <+> align (vsep (map pretty trueDeps)))
            <> line
            <> indent 2 ("else" <+> align (vsep (map pretty falseDeps)))
  pretty (ComponentStanza (SN _pn stanza) trueDeps)
    | null trueDeps =
        mempty
    | otherwise =
        "when"
          <+> fromString (showStanza stanza)
          <+> "is enabled"
            <> line
            <> indent 2 (vsep (map pretty trueDeps))

prettyComponentFlaggedDeps :: Cabal.Pretty qpn => ComponentFlaggedDeps qpn -> Doc ann
prettyComponentFlaggedDeps deps =
  align (vsep [pretty dep | dep <- deps, not (isNull dep)])
  where
    isNull (ComponentFlagged _fn _fInfo trueDeps falseDeps) | null trueDeps && null falseDeps = True
    isNull (ComponentStanza _sn trueDeps) | null trueDeps = True
    isNull _otherwise = False

-- flaggedDepQPN :: Traversal (FlaggedDep a) (FlaggedDep b) a b
-- flaggedDepQPN f (Simple lDep component) =
--   Simple <$> lDepQPN f lDep <*> pure component
-- flaggedDepQPN f (Flagged fn fInfo trueDeps falseDeps) =
--   Flagged <$> fnQPN f fn <*> pure fInfo <*> (traverse . flaggedDepQPN) f trueDeps <*> (traverse . flaggedDepQPN) f falseDeps
-- flaggedDepQPN f (Stanza sn trueDeps) =
--   Stanza <$> snQPN f sn <*> (traverse . flaggedDepQPN) f trueDeps
--
-- flaggedDepComponent :: Traversal' (FlaggedDep qpn) Component
-- flaggedDepComponent f (Simple lDep component) =
--   Simple lDep <$> f component
-- flaggedDepComponent f (Flagged fn fInfo trueDeps falseDeps) =
--   Flagged fn fInfo <$> (traverse . flaggedDepComponent) f trueDeps <*> (traverse . flaggedDepComponent) f falseDeps
-- flaggedDepComponent f (Stanza sn trueDeps) =
--   Stanza sn <$> (traverse . flaggedDepComponent) f trueDeps
-- lDepQPN :: Traversal (LDep a) (LDep b) a b
-- lDepQPN f (LDep depr dep) = LDep <$> dependecyReasonQPN f depr <*> depQPN f dep
--
-- depQPN :: Traversal (Dep a) (Dep b) a b
-- depQPN f (Dep (PkgComponent qpn ec) ci) = Dep <$> (PkgComponent <$> f qpn <*> pure ec) <*> pure ci
-- depQPN _ (Ext e) = pure (Ext e)
-- depQPN _ (Lang l) = pure (Lang l)
-- depQPN _ (Pkg pn pv) = pure (Pkg pn pv)
--
-- dependecyReasonQPN :: Traversal (DependencyReason a) (DependencyReason b) a b
-- dependecyReasonQPN f (DependencyReason qpn m s) = DependencyReason <$> f qpn <*> pure m <*> pure s
--
-- fnQPN :: Traversal (FN a) (FN b) a b
-- fnQPN f (FN qpn flag) = FN <$> f qpn <*> pure flag
--
-- snQPN :: Traversal (SN a) (SN b) a b
-- snQPN f (SN qpn stanza) = SN <$> f qpn <*> pure stanza

printSolverIndex :: Cabal.Pretty pn => Map pn (Map I PInfo) -> IO ()
printSolverIndex idx = do
  putStrLn "------------------------"
  putStrLn "----- solver index -----"
  putStrLn "------------------------"
  putStrLn ""

  for_ (Map.toList idx) $ \(pn, mipi) ->
    for_ (Map.toList mipi) $ \(i, PInfo deps mec flagInfo _mfail) -> do
      print $
        vsep $
          [ fromString $ Cabal.prettyShow pn ++ "-" ++ showI i,
            "exposed components:" <+> hsep (punctuate comma [prettyExposedComponentInfo ec ci | (ec, ci) <- Map.toList mec])
          ]
            ++ [ "flags:" <+> hsep (punctuate comma [(fromString (Cabal.prettyShow fn)) | (fn, _fi) <- Map.toList flagInfo])
               ]
            ++ [ "dependencies:",
                 indent 2 $
                   vsep
                     [ vsep
                         [ fromString (Cabal.prettyShow c) <+> "depends on",
                           indent 2 $ prettyComponentFlaggedDeps dep,
                           line
                         ]
                       | (c, dep) <- (Map.toList $ toComponents deps)
                     ]
               ]
