{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Aeson as Aeson hiding (Key, Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (first, second)
import Data.Binary
import Data.Binary.Get
import Data.Bitraversable (bisequence, bitraverse)
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Invariant (invmap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Tuple (swap)
import Data.Unjson
import Distribution.Backpack (DefUnitId, OpenModule, OpenUnitId)
import Distribution.Backpack.ModuleShape
import Distribution.Client.FileMonitor
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types (ElaboratedPackage)
import Distribution.Client.Types
import Distribution.Compat.Newtype
import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.InstallDirs (PathTemplate, fromPathTemplate, toPathTemplate)
import Distribution.System
import Distribution.Types.Flag
import Distribution.Types.UnitId
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Utils.Structured hiding (typeName)
import GHC.Generics
import System.IO
import Type.Reflection
import Data.Data (Data)
import Distribution.Client.PackageHash

unjsonElaboratedConfiguredPackage :: UnjsonDef ElaboratedConfiguredPackage
unjsonElaboratedConfiguredPackage =
  objectOf $
    ElaboratedConfiguredPackage
      <$> fieldBy "elabUnitId/" elabUnitId "elabUnitId" unjsonPrettyParsec
      <*> fieldBy "elabComponentId/" elabComponentId "elabComponentId" unjsonPrettyParsec
      <*> fieldBy "elabInstantiatedWith/" elabInstantiatedWith "elabInstantiatedWith" (unjsonModuleNameMapWith unjsonPrettyParsec)
      <*> fieldBy "elabLinkedInstantiatedWith/" elabLinkedInstantiatedWith "elabLinkedInstantiatedWith" (unjsonModuleNameMapWith unjsonPrettyParsec)
      <*> fieldBy "elabIsCanonical/" elabIsCanonical "elabIsCanonical" unjsonPrettyParsec
      <*> fieldBy "elabPkgSourceId/" elabPkgSourceId "elabPkgSourceId" unjsonPrettyParsec
      <*> fieldBy "elabModuleShape/" elabModuleShape "elabModuleShape" unjsonModuleShape
      <*> fieldBy "elabFlagAssignment/" elabFlagAssignment "elabFlagAssignment" unjsonPrettyParsec
      <*> fieldBy "elabFlagDefaults/" elabFlagDefaults "elabFlagDefaults" unjsonPrettyParsec
      <*> fieldBy "elabPkgDescription/" elabPkgDescription "elabPkgDescription" unjsonData
      <*> fieldBy "elabPkgSourceLocation/" elabPkgSourceLocation "elabPkgSourceLocation" unjsonPackageLocation
      <*> fieldOptBy "elabPkgSourceHash/" elabPkgSourceHash "elabPkgSourceHash" unjsonPkgSourceHash
      <*> fieldBy "elabLocalToProject/" elabLocalToProject "elabLocalToProject" unjsonPrettyParsec
      <*> fieldBy "elabBuildStyle/" elabBuildStyle "elabBuildStyle" unjsonBuildStyle
      <*> fieldBy "elabEnabledSpec/" elabEnabledSpec "elabEnabledSpec" unjsonPrettyParsec
      <*> fieldBy "elabStanzasAvailable/" elabStanzasAvailable "elabStanzasAvailable" unjsonPrettyParsec
      <*> fieldBy "elabStanzasRequested/" elabStanzasRequested "elabStanzasRequested" unjsonPrettyParsec
      <*> fieldBy "elabPackageDbs/" elabPackageDbs "elabPackageDbs" unjsonPrettyParsec
      <*> fieldBy "elabSetupPackageDBStack/" elabSetupPackageDBStack "elabSetupPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabBuildPackageDBStack/" elabBuildPackageDBStack "elabBuildPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabRegisterPackageDBStack/" elabRegisterPackageDBStack "elabRegisterPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabInplaceSetupPackageDBStack/" elabInplaceSetupPackageDBStack "elabInplaceSetupPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabInplaceBuildPackageDBStack/" elabInplaceBuildPackageDBStack "elabInplaceBuildPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabInplaceRegisterPackageDBStack/" elabInplaceRegisterPackageDBStack "elabInplaceRegisterPackageDBStack" unjsonPrettyParsec
      <*> fieldBy "elabPkgDescriptionOverride/" elabPkgDescriptionOverride "elabPkgDescriptionOverride" unjsonPrettyParsec
      <*> fieldBy "elabVanillaLib/" elabVanillaLib "elabVanillaLib" unjsonPrettyParsec
      <*> fieldBy "elabSharedLib/" elabSharedLib "elabSharedLib" unjsonPrettyParsec
      <*> fieldBy "elabStaticLib/" elabStaticLib "elabStaticLib" unjsonPrettyParsec
      <*> fieldBy "elabDynExe/" elabDynExe "elabDynExe" unjsonPrettyParsec
      <*> fieldBy "elabFullyStaticExe/" elabFullyStaticExe "elabFullyStaticExe" unjsonPrettyParsec
      <*> fieldBy "elabGHCiLib/" elabGHCiLib "elabGHCiLib" unjsonPrettyParsec
      <*> fieldBy "elabProfLib/" elabProfLib "elabProfLib" unjsonPrettyParsec
      <*> fieldBy "elabProfExe/" elabProfExe "elabProfExe" unjsonPrettyParsec
      <*> fieldBy "elabProfLibDetail/" elabProfLibDetail "elabProfLibDetail" unjsonPrettyParsec
      <*> fieldBy "elabProfExeDetail/" elabProfExeDetail "elabProfExeDetail" unjsonPrettyParsec
      <*> fieldBy "elabCoverage/" elabCoverage "elabCoverage" unjsonPrettyParsec
      <*> fieldBy "elabOptimization/" elabOptimization "elabOptimization" unjsonPrettyParsec
      <*> fieldBy "elabSplitObjs/" elabSplitObjs "elabSplitObjs" unjsonPrettyParsec
      <*> fieldBy "elabSplitSections/" elabSplitSections "elabSplitSections" unjsonPrettyParsec
      <*> fieldBy "elabStripLibs/" elabStripLibs "elabStripLibs" unjsonPrettyParsec
      <*> fieldBy "elabStripExes/" elabStripExes "elabStripExes" unjsonPrettyParsec
      <*> fieldBy "elabDebugInfo/" elabDebugInfo "elabDebugInfo" unjsonPrettyParsec
      <*> fieldBy "elabDumpBuildInfo/" elabDumpBuildInfo "elabDumpBuildInfo" unjsonPrettyParsec
      <*> fieldBy "elabProgramPaths/" elabProgramPaths "elabProgramPaths" unjsonPrettyParsec
      <*> fieldBy "elabProgramArgs/" elabProgramArgs "elabProgramArgs" unjsonPrettyParsec
      <*> fieldBy "elabProgramPathExtra/" elabProgramPathExtra "elabProgramPathExtra" unjsonPrettyParsec
      <*> fieldBy "elabConfigureScriptArgs/" elabConfigureScriptArgs "elabConfigureScriptArgs" unjsonPrettyParsec
      <*> fieldBy "elabExtraLibDirs/" elabExtraLibDirs "elabExtraLibDirs" unjsonPrettyParsec
      <*> fieldBy "elabExtraLibDirsStatic/" elabExtraLibDirsStatic "elabExtraLibDirsStatic" unjsonPrettyParsec
      <*> fieldBy "elabExtraFrameworkDirs/" elabExtraFrameworkDirs "elabExtraFrameworkDirs" unjsonPrettyParsec
      <*> fieldBy "elabExtraIncludeDirs/" elabExtraIncludeDirs "elabExtraIncludeDirs" unjsonPrettyParsec
      <*> fieldBy "elabProgPrefix/" elabProgPrefix "elabProgPrefix" unjsonPrettyParsec
      <*> fieldBy "elabProgSuffix/" elabProgSuffix "elabProgSuffix" unjsonPrettyParsec
      <*> fieldBy "elabInstallDirs/" elabInstallDirs "elabInstallDirs" unjsonPrettyParsec
      <*> fieldBy "elabHaddockHoogle/" elabHaddockHoogle "elabHaddockHoogle" unjsonPrettyParsec
      <*> fieldBy "elabHaddockHtml/" elabHaddockHtml "elabHaddockHtml" unjsonPrettyParsec
      <*> fieldBy "elabHaddockHtmlLocation/" elabHaddockHtmlLocation "elabHaddockHtmlLocation" unjsonPrettyParsec
      <*> fieldBy "elabHaddockForeignLibs/" elabHaddockForeignLibs "elabHaddockForeignLibs" unjsonPrettyParsec
      <*> fieldBy "elabHaddockForHackage/" elabHaddockForHackage "elabHaddockForHackage" unjsonPrettyParsec
      <*> fieldBy "elabHaddockExecutables/" elabHaddockExecutables "elabHaddockExecutables" unjsonPrettyParsec
      <*> fieldBy "elabHaddockTestSuites/" elabHaddockTestSuites "elabHaddockTestSuites" unjsonPrettyParsec
      <*> fieldBy "elabHaddockBenchmarks/" elabHaddockBenchmarks "elabHaddockBenchmarks" unjsonPrettyParsec
      <*> fieldBy "elabHaddockInternal/" elabHaddockInternal "elabHaddockInternal" unjsonPrettyParsec
      <*> fieldBy "elabHaddockCss/" elabHaddockCss "elabHaddockCss" unjsonPrettyParsec
      <*> fieldBy "elabHaddockLinkedSource/" elabHaddockLinkedSource "elabHaddockLinkedSource" unjsonPrettyParsec
      <*> fieldBy "elabHaddockQuickJump/" elabHaddockQuickJump "elabHaddockQuickJump" unjsonPrettyParsec
      <*> fieldBy "elabHaddockHscolourCss/" elabHaddockHscolourCss "elabHaddockHscolourCss" unjsonPrettyParsec
      <*> fieldBy "elabHaddockContents/" elabHaddockContents "elabHaddockContents" unjsonPrettyParsec
      <*> fieldBy "elabHaddockIndex/" elabHaddockIndex "elabHaddockIndex" unjsonPrettyParsec
      <*> fieldBy "elabHaddockBaseUrl/" elabHaddockBaseUrl "elabHaddockBaseUrl" unjsonPrettyParsec
      <*> fieldBy "elabHaddockLib/" elabHaddockLib "elabHaddockLib" unjsonPrettyParsec
      <*> fieldBy "elabTestMachineLog/" elabTestMachineLog "elabTestMachineLog" unjsonPrettyParsec
      <*> fieldBy "elabTestHumanLog/" elabTestHumanLog "elabTestHumanLog" unjsonPrettyParsec
      <*> fieldOptBy "elabTestShowDetails/" elabTestShowDetails "elabTestShowDetails" unjsonPrettyParsec
      <*> fieldBy "elabTestKeepTix/" elabTestKeepTix "elabTestKeepTix" unjsonPrettyParsec
      <*> fieldBy "elabTestWrapper/" elabTestWrapper "elabTestWrapper" unjsonPrettyParsec
      <*> fieldBy "elabTestFailWhenNoTestSuites/" elabTestFailWhenNoTestSuites "elabTestFailWhenNoTestSuites" unjsonPrettyParsec
      <*> fieldBy "elabTestTestOptions/" elabTestTestOptions "elabTestTestOptions" (arrayOf unjsonPathTemplate)
      <*> fieldBy "elabBenchmarkOptions/" elabBenchmarkOptions "elabBenchmarkOptions" (arrayOf unjsonPathTemplate)
      <*> fieldBy "elabSetupScriptStyle/" elabSetupScriptStyle "elabSetupScriptStyle" unjsonPrettyParsec
      <*> fieldBy "elabSetupScriptCliVersion/" elabSetupScriptCliVersion "elabSetupScriptCliVersion" unjsonPrettyParsec
      <*> fieldBy "elabConfigureTargets/" elabConfigureTargets "elabConfigureTargets" unjsonPrettyParsec
      <*> fieldBy "elabBuildTargets/" elabBuildTargets "elabBuildTargets" unjsonPrettyParsec
      <*> fieldBy "elabTestTargets/" elabTestTargets "elabTestTargets" unjsonPrettyParsec
      <*> fieldBy "elabBenchTargets/" elabBenchTargets "elabBenchTargets" unjsonPrettyParsec
      <*> fieldBy "elabReplTarget/" elabReplTarget "elabReplTarget" unjsonPrettyParsec
      <*> fieldBy "elabHaddockTargets/" elabHaddockTargets "elabHaddockTargets" unjsonPrettyParsec
      <*> fieldBy "elabBuildHaddocks/" elabBuildHaddocks "elabBuildHaddocks" unjsonPrettyParsec
      <*> fieldBy "elabPkgOrComp/" elabPkgOrComp "elabPkgOrComp" unjsonPrettyParsec

unjsonBuildStyle :: UnjsonDef BuildStyle
unjsonBuildStyle = _

unjsonPkgSourceHash :: UnjsonDef PackageSourceHash
unjsonPkgSourceHash = _

unjsonPackageLocation :: UnjsonDef (PackageLocation local)
unjsonPackageLocation = _

unjsonData :: Data a => UnjsonDef a
unjsonData = _

unjsonModuleShape :: UnjsonDef ModuleShape
unjsonModuleShape = _

unjsonModuleNameMapWith :: forall v. Typeable v => UnjsonDef v -> UnjsonDef (Map ModuleName v)
unjsonModuleNameMapWith u =
  unjsonInvmapR
    ( either fail ((`Result` []) . M.fromList)
        . traverse (bitraverse (eitherParsec @ModuleName . Key.toString) pure)
        . KeyMap.toList
    )
    (KeyMap.fromMap . M.mapKeys (Key.fromString . prettyShow))
    (mapOf u)

mapOf' :: Typeable v => UnjsonDef v -> UnjsonDef (KeyMap.KeyMap v)
mapOf' def = MapUnjsonDef def pure id

unjsonPathTemplate :: UnjsonDef PathTemplate
unjsonPathTemplate = invmap toPathTemplate fromPathTemplate unjsonDef

unjsonPrettyParsec :: (Parsec a, Pretty a) => UnjsonDef a
unjsonPrettyParsec =
  unjsonInvmapR
    (\str -> case eitherParsec str of Right result -> Result result []; Left e -> fail e)
    prettyShow
    unjsonDef

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
