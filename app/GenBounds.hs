{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map.Strict qualified as Map
import Distribution.Client.Compat.Prelude
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.PackageSpecifier
import Distribution.Package (packageName, packageVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (transformAllBuildDepends)
import Distribution.PackageDescription.PrettyPrint
import Distribution.Solver.Types.SourcePackage
import Distribution.Verbosity qualified as Verbosity
import Distribution.Version hiding (hasLowerBound)
import System.FilePath
import Prelude ()

main :: IO ()
main = do
    let verbosity = Verbosity.normal

    ProjectBaseContext
        { distDirLayout
        , cabalDirLayout
        , projectConfig
        , localPackages
        } <-
        establishProjectBaseContext verbosity mempty OtherCommand

    (_, elaboratedPlan, _, _, _) <-
        rebuildInstallPlan
            verbosity
            distDirLayout
            cabalDirLayout
            projectConfig
            localPackages

    let versionsMap =
            Map.fromList
                $ [ (packageName ipi, packageVersion ipi)
                  | InstallPlan.PreExisting ipi <- InstallPlan.toList elaboratedPlan
                  ]
                ++ [ (packageName ecp, packageVersion ecp)
                   | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan
                   , not $ elabLocalToProject ecp
                   ]

    let improveDepedency dep@(Dependency pn vr nes) =
            case Map.lookup pn versionsMap of
                Nothing -> dep
                Just v -> Dependency pn (improveVersionRange vr v) nes

    for_ localPackages $ \case
        SpecificSourcePackage SourcePackage{srcpkgPackageId, srcpkgSource = LocalUnpackedPackage pkgPath, srcpkgDescription, srcpkgDescrOverride = Nothing} -> do
            let PackageIdentifier{pkgName} = srcpkgPackageId

            let fp = pkgPath </> unPackageName pkgName <.> "cabal.improved"
            let srcpkgDescription' = transformAllBuildDepends improveDepedency srcpkgDescription

            putStrLn $ "Writing " ++ fp
            writeGenericPackageDescription fp srcpkgDescription'
        anyOtherCase ->
            putStrLn $ "Not handled: " ++ show anyOtherCase

improveVersionRange :: VersionRange -> Version -> VersionRange
improveVersionRange vr v =
    simplifyVersionRange . addMV v . addUB v . addLB v $ vr

addLB :: Version -> VersionRange -> VersionRange
addLB v vr
    | hasLowerBound vr = vr
    | otherwise = intersectVersionRanges vr (laterVersion v)

addUB :: Version -> VersionRange -> VersionRange
addUB v vr
    | hasUpperBound vr = vr
    | otherwise = intersectVersionRanges vr (earlierVersion $ majorUpperBound v)

addMV :: Version -> VersionRange -> VersionRange
addMV v vr = unionVersionRanges vr (majorBoundVersion v)

-- The documentation of Distribution.Types.VersioRange.hasLowerBound says
-- > Note: this function only considers the user-specified lower bounds,
-- > but not the implicit >=0 lower bound.
-- but hasLowerBound (orLaterVersion $ mkVersion [0]) = True
-- This function should give the right answer
hasLowerBound :: VersionRange -> Bool
hasLowerBound = not . withinRange (mkVersion [0])
