{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Binary (Binary (get), Get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString.Lazy qualified as BS
import Data.Map
import Distribution.Client.DistDirLayout (DistDirLayout (DistDirLayout, distProjectCacheFile), defaultDistDirLayout)
import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.ProjectConfig (SolverSettings, findProjectRoot)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Distribution.Simple (Compiler, PackageName)
import Distribution.Simple.Program (ConfiguredProgram)
import Distribution.Solver.Types.OptionalStanza (OptionalStanza)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.System (Platform)
import Distribution.Utils.Structured (Structured, Tag)
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Pretty.Simple (pPrintOpt, OutputOptions (outputOptionsCompact, outputOptionsCompactParens), CheckColorTty (NoCheckColorTty), defaultOutputOptionsDarkBg)
import qualified Distribution.Client.SolverInstallPlan as SIP
import Data.Foldable (for_)
import Distribution.Solver.Types.SourcePackage (SourcePackage(srcpkgPackageId))

type Key = (SolverSettings, [PackageSpecifier UnresolvedSourcePackage], Map PackageName (Map OptionalStanza Bool), Compiler, Platform, [ConfiguredProgram])

type Value = (SolverInstallPlan, PkgConfigDb, TotalIndexState, ActiveRepos)

pPrint' :: Show a => a -> IO ()
pPrint' = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg { outputOptionsCompact = True, outputOptionsCompactParens = True }

main :: IO ()
main = do
  Right prjRoot <- findProjectRoot Nothing Nothing
  let DistDirLayout {distProjectCacheFile} = defaultDistDirLayout prjRoot Nothing

  withCacheFile @Key @Value (distProjectCacheFile "solver-plan") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right v) -> do
      let (solverSettings, localPackages, localPackagesEnabledStanzas, compiler, platform, configuredPrograms) = k
      let (solverPlan, _pkgConfigDB, totalIndexState, activeRepos) = v

      putStrLn "-------------------- solverSettings --------------------"
      pPrint' solverSettings

      putStrLn "-------------------- localPackages --------------------"
      for_ localPackages $ pPrint' . fmap srcpkgPackageId

      putStrLn "-------------------- localPackagesEnabledStanzas --------------------"
      pPrint' localPackagesEnabledStanzas

      putStrLn "-------------------- compiler --------------------"
      pPrint' compiler

      putStrLn "-------------------- platform --------------------"
      pPrint' platform

      putStrLn "-------------------- configuredPrograms --------------------"
      pPrint' configuredPrograms

      putStrLn "-------------------- solverPlan --------------------"
      for_ (SIP.toList solverPlan) pPrint'

      putStrLn "-------------------- totalIndexState --------------------"
      pPrint' totalIndexState

      putStrLn "-------------------- activeRepos --------------------"
      pPrint' activeRepos

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
