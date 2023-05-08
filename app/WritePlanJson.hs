{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Binary (Binary (get), Get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as BS
import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile), defaultDistDirLayout)
import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.ProjectConfig (ProjectConfig, findProjectRoot)
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, ElaboratedSharedConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Distribution.Utils.Structured (Structured, Tag)
import System.IO (IOMode (ReadMode), withBinaryFile)

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

main :: IO ()
main = do
  Right prjRoot <- findProjectRoot Nothing Nothing
  let distDirLayout = defaultDistDirLayout prjRoot Nothing

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
