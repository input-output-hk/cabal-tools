{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (when)
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types
import Distribution.InstalledPackageInfo
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.LocalBuildInfo
import Distribution.Version (Bound (..), UpperBound (..), VersionInterval (..), asVersionIntervals)
import Opts (parseOpts)
import WithCacheFile (withCacheFile)

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

main :: IO ()
main = do
  (_prjRoot, distDirLayout) <- parseOpts

  withCacheFile @Key @Value (distProjectCacheFile distDirLayout "elaborated-plan") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right v) -> do
      let (_projectConfig, _localPackages, _progSearchPath) = k :: Key
      let (elaboratedInstallPlan, _elaboratedSharedConfig, _totalIndexState, _activeRepos) = v :: Value

      let upperBoundsMap = Map.fromListWith (<>) $ do
            planPackage <- toList elaboratedInstallPlan

            foldPlanPackage
              (\InstalledPackageInfo {} -> [])
              ( \ElaboratedConfiguredPackage {elabPkgSourceId, elabPkgDescription} -> do
                  component <- pkgComponents elabPkgDescription
                  Dependency pkgName versionRange _libraryNames <- targetBuildDepends $ componentBuildInfo component
                  let maxUpperBound = maximum $ map (\(VersionInterval _lb ub) -> UpperBound'Ord ub) $ asVersionIntervals versionRange
                  return (pkgName, [(maxUpperBound, (elabPkgSourceId, componentName component))])
              )
              planPackage

      for_ (Map.toList upperBoundsMap) $ \(pkgName, ubs) -> do
        let (ub, cs) = groupByMin ubs
        let componentMap = groupBy cs

        when (coerce ub /= NoUpperBound) $ do
          putStrLn $
            unlines $
              unwords [prettyShow pkgName, "is constrainted to", showUpper (coerce ub), "by:"]
                : [ unwords
                      [ "\t",
                        prettyShow pkgId,
                        concat
                          [ "(",
                            -- intercalate ", " (map (showComponentName . componentName) components),
                            intercalate ", " (map prettyShow components),
                            ")"
                          ]
                      ]
                    | (pkgId, components) <- componentMap
                  ]

groupBy :: Ord k => [(k, v)] -> [(k, [v])]
groupBy = Map.toList . Map.fromListWith (<>) . map (second (: []))

groupByMin :: Ord k => [(k, v)] -> (k, [v])
groupByMin = Map.findMin . Map.fromListWith (<>) . map (second (: []))

-- choseMinSnd :: Ord b => (a, b) -> (a, b) -> (a, b)
-- choseMinSnd (a1, b1) (a2, b2)
--   | b1 < b2 = (a1, b1)
--   | otherwise = (a2, b2)

-- upperBound :: VersionInterval -> UpperBound
-- upperBound (VersionInterval _ ub) = ub

showUpper :: UpperBound -> String
showUpper (UpperBound ver ExclusiveBound) = "<" <> prettyShow ver
showUpper (UpperBound ver InclusiveBound) = "<=" <> prettyShow ver
showUpper NoUpperBound = "∞"

-- showLower :: LowerBound -> String
-- showLower (LowerBound ver ExclusiveBound) = ">" <> prettyShow ver
-- showLower (LowerBound ver InclusiveBound) = ">=" <> prettyShow ver

-- showInterval :: VersionInterval -> String
-- showInterval (VersionInterval lb up) = unwords ["from", showLower lb, "to", showUpper up]

newtype UpperBound'Ord = UpperBound'Ord UpperBound
  deriving (Eq, Show)

instance Ord UpperBound'Ord where
  (UpperBound'Ord NoUpperBound) `compare` (UpperBound'Ord NoUpperBound) = EQ
  (UpperBound'Ord (UpperBound _b1 _bound1)) `compare` (UpperBound'Ord NoUpperBound) = LT
  (UpperBound'Ord NoUpperBound) `compare` (UpperBound'Ord (UpperBound _b2 _bound2)) = GT
  (UpperBound'Ord (UpperBound b1 bound1)) `compare` (UpperBound'Ord (UpperBound b2 bound2)) =
    case compare b1 b2 of
      LT -> LT
      GT -> GT
      EQ -> case (bound1, bound2) of
        (ExclusiveBound, InclusiveBound) -> LT
        (InclusiveBound, ExclusiveBound) -> GT
        _otherwise -> EQ
