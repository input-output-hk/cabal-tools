{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, ElaboratedSharedConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Opts (parseOpts)
import WithCacheFile (withCacheFile)

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

main :: IO ()
main = do
  (_prjRoot, distDirLayout) <- parseOpts

  withCacheFile @Key @Value (distProjectCacheFile distDirLayout "improved-plan") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, _k, Right v) -> do
      let (improvedPlan, elaboratedPlan, elaboratedSharedConfig, _totalIndexState, _activeRepos) = v
      putStrLn "writing elaborated-plan.json"
      writePlanExternalRepresentation (distDirLayout {distProjectCacheFile = const "elaborated-plan.json"}) elaboratedPlan elaboratedSharedConfig

      putStrLn "writing improved-plan.json"
      writePlanExternalRepresentation (distDirLayout {distProjectCacheFile = const "improved-plan.json"}) improvedPlan elaboratedSharedConfig
