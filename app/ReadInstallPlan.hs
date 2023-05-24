{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (for_)
import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.InstallPlan (toList)
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, ElaboratedSharedConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Opts (parseOpts)
import PrettyPrintSimple (pPrint)
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
    Right (_monitorStateFileSet, k, Right v) -> do
      let (projectConfig, localPackages, progSearchPath) = k
      let (improvedPlan, elaboratedPlan, elaboratedSharedConfig, totalIndexState, activeRepos) = v

      putStrLn "-------------------- projectConfig --------------------"
      pPrint projectConfig

      putStrLn "-------------------- localPackages --------------------"
      pPrint localPackages

      putStrLn "-------------------- progSearchPath --------------------"
      pPrint progSearchPath

      putStrLn "-------------------- elaboratedInstallPlan --------------------"
      for_ (toList elaboratedPlan) pPrint

      putStrLn "-------------------- elaboratedInstallPlan --------------------"
      for_ (toList improvedPlan) pPrint

      putStrLn "-------------------- elaboratedSharedConfig --------------------"
      pPrint elaboratedSharedConfig

      putStrLn "-------------------- totalIndexState --------------------"
      pPrint totalIndexState

      putStrLn "-------------------- activeRepos --------------------"
      pPrint activeRepos
