{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Opts (parseOpts)
import PrettyPrint (pPrint)
import WithCacheFile (withCacheFile)

type Key = (FilePath, FilePath)

type Value = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage])

main :: IO ()
main = do
  (_prjRoot, distDirLayout) <- parseOpts

  withCacheFile @Key @Value (distProjectCacheFile distDirLayout "config") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right v) -> do
      let (configPath, projectFile) = k
      let (projectConfig, localPackages) = v

      pPrint configPath
      pPrint projectFile
      pPrint projectConfig
      pPrint localPackages
