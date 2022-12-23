{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BS
import Distribution.Client.FileMonitor
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types
import Distribution.Utils.Structured
import System.IO

main :: IO ()
main = do
  withCacheFile "dist-newstyle/cache/elaborated-plan" $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right v) -> do
      let (projectConfig, localPackages, progSearchPath) = k :: Key
      let (elaboratedInstallPlan, elaboratedSharedConfig, totalIndexState, activeRepos) = v :: Value

      putStrLn "-------------------- projectConfig --------------------"
      print projectConfig

      putStrLn "-------------------- localPackages --------------------"
      print localPackages

      putStrLn "-------------------- progSearchPath --------------------"
      print progSearchPath

      putStrLn "-------------------- elaboratedInstallPlan --------------------"
      print $ toList elaboratedInstallPlan

      putStrLn "-------------------- elaboratedSharedConfig --------------------"
      print elaboratedSharedConfig

      putStrLn "-------------------- totalIndexState --------------------"
      print totalIndexState

      putStrLn "-------------------- activeRepos --------------------"
      print activeRepos

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

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
