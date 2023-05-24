module Dependency where

import Data.Map.Strict qualified as Map
import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Compat.Lens (over)
import Distribution.Package (Package (..), packageName)
import Distribution.PackageDescription.Configuration (transformAllBuildDepends)
import Distribution.Solver.Types.PackageIndex qualified as Solver.PackageIndex
import Distribution.Types.Dependency
import Distribution.Types.GenericPackageDescription
import Distribution.Types.VersionRange
import Distribution.Version (removeLowerBound, removeUpperBound, transformCaretLower, transformCaretUpper)
import SourcePackage.Lens (srcpkgDescription)

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
  GenericPackageDescription ->
  GenericPackageDescription
relaxPackageDeps relKind RelaxDepsAll gpd =
  transformAllBuildDepends relaxAll gpd
  where
    relaxAll :: Dependency -> Dependency
    relaxAll (Dependency pkgName verRange cs) =
      Dependency pkgName (removeBound relKind RelaxDepModNone verRange) cs
relaxPackageDeps relKind (RelaxDepsSome depsToRelax0) gpd =
  transformAllBuildDepends relaxSome gpd
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
