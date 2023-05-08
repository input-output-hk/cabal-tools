{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Binary (Binary (get), Get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (for_)
import Distribution.Client.DistDirLayout (DistDirLayout (DistDirLayout, distProjectCacheFile), defaultDistDirLayout)
import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.InstallPlan (toList)
import Distribution.Client.ProjectConfig (ProjectConfig, findProjectRoot)
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, ElaboratedSharedConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Distribution.Utils.Structured (Structured, Tag)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Pretty.Simple (pPrint)

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

main :: IO ()
main = do
  Right prjRoot <- findProjectRoot Nothing Nothing
  let DistDirLayout {distProjectCacheFile} = defaultDistDirLayout prjRoot Nothing

  withCacheFile @Key @Value (distProjectCacheFile "improved-plan") $ \case
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

withCacheFile ::
  (Binary a, Structured a, Binary b, Structured b) =>
  FilePath ->
  (Either String (MonitorStateFileSet, a, Either String b) -> IO r) ->
  IO r
withCacheFile cacheFile k =
  withBinaryFile cacheFile ReadMode $ \hnd -> do
    contents <- structuredDecodeTriple <$> BS.hGetContents hnd
    k contents

structuredDecodeTriple ::
  forall a b c.
  (Structured a, Structured b, Structured c, Binary a, Binary b, Binary c) =>
  BS.ByteString ->
  Either String (a, b, Either String c)
structuredDecodeTriple lbs =
  let partialDecode =
        (`runGetOrFail` lbs) $ do
          (_ :: Tag (a, b, c)) <- get
          (a :: a) <- get
          (b :: b) <- get
          pure (a, b)
      cleanEither (Left (_, pos, msg)) = Left ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)
      cleanEither (Right (_, _, v)) = Right v
   in case partialDecode of
        Left (_, pos, msg) -> Left ("Data.Binary.Get.runGet at position " ++ show pos ++ ": " ++ msg)
        Right (lbs', _, (x, y)) -> Right (x, y, cleanEither $ runGetOrFail (get :: Get c) lbs')
