{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy qualified as BS
import Data.Functor.Invariant (invmap)
import Data.Proxy
import Data.Text
import Data.Text qualified as Text
import Data.Typeable
import Data.Unjson
import Distribution.Client.FileMonitor
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.InstallDirs (PathTemplate, fromPathTemplate, toPathTemplate)
import Distribution.Utils.Structured hiding (typeName)
import System.IO

-- unjsonElaboratedConfiguredPackage :: UnjsonDef ElaboratedConfiguredPackage
-- unjsonElaboratedConfiguredPackage =
--   objectOf $
--     ElaboratedConfiguredPackage
--       <$> fieldBy "elabUnitId/" elabUnitId "elabUnitId" unjsonPrettyParsec
--       <*> fieldBy "elabComponentId/" elabComponentId "elabComponentId" unjsonPrettyParsec
--       <*> fieldBy "elabInstantiatedWith/" elabInstantiatedWith "elabInstantiatedWith" unjsonPrettyParsec
--       <*> fieldBy "elabLinkedInstantiatedWith/" elabLinkedInstantiatedWith "elabLinkedInstantiatedWith" unjsonPrettyParsec
--       <*> fieldBy "elabIsCanonical/" elabIsCanonical "elabIsCanonical" unjsonPrettyParsec
--       <*> fieldBy "elabPkgSourceId/" elabPkgSourceId "elabPkgSourceId" unjsonPrettyParsec
--       <*> fieldBy "elabModuleShape/" elabModuleShape "elabModuleShape" unjsonPrettyParsec
--       <*> fieldBy "elabFlagAssignment/" elabFlagAssignment "elabFlagAssignment" unjsonPrettyParsec
--       <*> fieldBy "elabFlagDefaults/" elabFlagDefaults "elabFlagDefaults" unjsonPrettyParsec
--       <*> fieldBy "elabPkgDescription/" elabPkgDescription "elabPkgDescription" unjsonPrettyParsec
--       <*> fieldBy "elabPkgSourceLocation/" elabPkgSourceLocation "elabPkgSourceLocation" unjsonPrettyParsec
--       <*> fieldBy "elabPkgSourceHash/" elabPkgSourceHash "elabPkgSourceHash" unjsonPrettyParsec
--       <*> fieldBy "elabLocalToProject/" elabLocalToProject "elabLocalToProject" unjsonPrettyParsec
--       <*> fieldBy "elabBuildStyle/" elabBuildStyle "elabBuildStyle" unjsonPrettyParsec
--       <*> fieldBy "elabEnabledSpec/" elabEnabledSpec "elabEnabledSpec" unjsonPrettyParsec
--       <*> fieldBy "elabStanzasAvailable/" elabStanzasAvailable "elabStanzasAvailable" unjsonPrettyParsec
--       <*> fieldBy "elabStanzasRequested/" elabStanzasRequested "elabStanzasRequested" unjsonPrettyParsec
--       <*> fieldBy "elabPackageDbs/" elabPackageDbs "elabPackageDbs" unjsonPrettyParsec
--       <*> fieldBy "elabSetupPackageDBStack/" elabSetupPackageDBStack "elabSetupPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabBuildPackageDBStack/" elabBuildPackageDBStack "elabBuildPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabRegisterPackageDBStack/" elabRegisterPackageDBStack "elabRegisterPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabInplaceSetupPackageDBStack/" elabInplaceSetupPackageDBStack "elabInplaceSetupPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabInplaceBuildPackageDBStack/" elabInplaceBuildPackageDBStack "elabInplaceBuildPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabInplaceRegisterPackageDBStack/" elabInplaceRegisterPackageDBStack "elabInplaceRegisterPackageDBStack" unjsonPrettyParsec
--       <*> fieldBy "elabPkgDescriptionOverride/" elabPkgDescriptionOverride "elabPkgDescriptionOverride" unjsonPrettyParsec
--       <*> fieldBy "elabVanillaLib/" elabVanillaLib "elabVanillaLib" unjsonPrettyParsec
--       <*> fieldBy "elabSharedLib/" elabSharedLib "elabSharedLib" unjsonPrettyParsec
--       <*> fieldBy "elabStaticLib/" elabStaticLib "elabStaticLib" unjsonPrettyParsec
--       <*> fieldBy "elabDynExe/" elabDynExe "elabDynExe" unjsonPrettyParsec
--       <*> fieldBy "elabFullyStaticExe/" elabFullyStaticExe "elabFullyStaticExe" unjsonPrettyParsec
--       <*> fieldBy "elabGHCiLib/" elabGHCiLib "elabGHCiLib" unjsonPrettyParsec
--       <*> fieldBy "elabProfLib/" elabProfLib "elabProfLib" unjsonPrettyParsec
--       <*> fieldBy "elabProfExe/" elabProfExe "elabProfExe" unjsonPrettyParsec
--       <*> fieldBy "elabProfLibDetail/" elabProfLibDetail "elabProfLibDetail" unjsonPrettyParsec
--       <*> fieldBy "elabProfExeDetail/" elabProfExeDetail "elabProfExeDetail" unjsonPrettyParsec
--       <*> fieldBy "elabCoverage/" elabCoverage "elabCoverage" unjsonPrettyParsec
--       <*> fieldBy "elabOptimization/" elabOptimization "elabOptimization" unjsonPrettyParsec
--       <*> fieldBy "elabSplitObjs/" elabSplitObjs "elabSplitObjs" unjsonPrettyParsec
--       <*> fieldBy "elabSplitSections/" elabSplitSections "elabSplitSections" unjsonPrettyParsec
--       <*> fieldBy "elabStripLibs/" elabStripLibs "elabStripLibs" unjsonPrettyParsec
--       <*> fieldBy "elabStripExes/" elabStripExes "elabStripExes" unjsonPrettyParsec
--       <*> fieldBy "elabDebugInfo/" elabDebugInfo "elabDebugInfo" unjsonPrettyParsec
--       <*> fieldBy "elabDumpBuildInfo/" elabDumpBuildInfo "elabDumpBuildInfo" unjsonPrettyParsec
--       <*> fieldBy "elabProgramPaths/" elabProgramPaths "elabProgramPaths" unjsonPrettyParsec
--       <*> fieldBy "elabProgramArgs/" elabProgramArgs "elabProgramArgs" unjsonPrettyParsec
--       <*> fieldBy "elabProgramPathExtra/" elabProgramPathExtra "elabProgramPathExtra" unjsonPrettyParsec
--       <*> fieldBy "elabConfigureScriptArgs/" elabConfigureScriptArgs "elabConfigureScriptArgs" unjsonPrettyParsec
--       <*> fieldBy "elabExtraLibDirs/" elabExtraLibDirs "elabExtraLibDirs" unjsonPrettyParsec
--       <*> fieldBy "elabExtraLibDirsStatic/" elabExtraLibDirsStatic "elabExtraLibDirsStatic" unjsonPrettyParsec
--       <*> fieldBy "elabExtraFrameworkDirs/" elabExtraFrameworkDirs "elabExtraFrameworkDirs" unjsonPrettyParsec
--       <*> fieldBy "elabExtraIncludeDirs/" elabExtraIncludeDirs "elabExtraIncludeDirs" unjsonPrettyParsec
--       <*> fieldBy "elabProgPrefix/" elabProgPrefix "elabProgPrefix" unjsonPrettyParsec
--       <*> fieldBy "elabProgSuffix/" elabProgSuffix "elabProgSuffix" unjsonPrettyParsec
--       <*> fieldBy "elabInstallDirs/" elabInstallDirs "elabInstallDirs" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockHoogle/" elabHaddockHoogle "elabHaddockHoogle" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockHtml/" elabHaddockHtml "elabHaddockHtml" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockHtmlLocation/" elabHaddockHtmlLocation "elabHaddockHtmlLocation" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockForeignLibs/" elabHaddockForeignLibs "elabHaddockForeignLibs" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockForHackage/" elabHaddockForHackage "elabHaddockForHackage" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockExecutables/" elabHaddockExecutables "elabHaddockExecutables" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockTestSuites/" elabHaddockTestSuites "elabHaddockTestSuites" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockBenchmarks/" elabHaddockBenchmarks "elabHaddockBenchmarks" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockInternal/" elabHaddockInternal "elabHaddockInternal" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockCss/" elabHaddockCss "elabHaddockCss" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockLinkedSource/" elabHaddockLinkedSource "elabHaddockLinkedSource" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockQuickJump/" elabHaddockQuickJump "elabHaddockQuickJump" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockHscolourCss/" elabHaddockHscolourCss "elabHaddockHscolourCss" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockContents/" elabHaddockContents "elabHaddockContents" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockIndex/" elabHaddockIndex "elabHaddockIndex" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockBaseUrl/" elabHaddockBaseUrl "elabHaddockBaseUrl" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockLib/" elabHaddockLib "elabHaddockLib" unjsonPrettyParsec
--       <*> fieldBy "elabTestMachineLog/" elabTestMachineLog "elabTestMachineLog" unjsonPrettyParsec
--       <*> fieldBy "elabTestHumanLog/" elabTestHumanLog "elabTestHumanLog" unjsonPrettyParsec
--       <*> fieldOptBy "elabTestShowDetails/" elabTestShowDetails "elabTestShowDetails" unjsonPrettyParsec
--       <*> fieldBy "elabTestKeepTix/" elabTestKeepTix "elabTestKeepTix" unjsonPrettyParsec
--       <*> fieldBy "elabTestWrapper/" elabTestWrapper "elabTestWrapper" unjsonPrettyParsec
--       <*> fieldBy "elabTestFailWhenNoTestSuites/" elabTestFailWhenNoTestSuites "elabTestFailWhenNoTestSuites" unjsonPrettyParsec
--       <*> fieldBy "elabTestTestOptions/" elabTestTestOptions "elabTestTestOptions" unjsonPrettyParsec
--       <*> fieldBy "elabBenchmarkOptions/" elabBenchmarkOptions "elabBenchmarkOptions" unjsonPrettyParsec
--       <*> fieldBy "elabSetupScriptStyle/" elabSetupScriptStyle "elabSetupScriptStyle" unjsonPrettyParsec
--       <*> fieldBy "elabSetupScriptCliVersion/" elabSetupScriptCliVersion "elabSetupScriptCliVersion" unjsonPrettyParsec
--       <*> fieldBy "elabConfigureTargets/" elabConfigureTargets "elabConfigureTargets" unjsonPrettyParsec
--       <*> fieldBy "elabBuildTargets/" elabBuildTargets "elabBuildTargets" unjsonPrettyParsec
--       <*> fieldBy "elabTestTargets/" elabTestTargets "elabTestTargets" unjsonPrettyParsec
--       <*> fieldBy "elabBenchTargets/" elabBenchTargets "elabBenchTargets" unjsonPrettyParsec
--       <*> fieldBy "elabReplTarget/" elabReplTarget "elabReplTarget" unjsonPrettyParsec
--       <*> fieldBy "elabHaddockTargets/" elabHaddockTargets "elabHaddockTargets" unjsonPrettyParsec
--       <*> fieldBy "elabBuildHaddocks/" elabBuildHaddocks "elabBuildHaddocks" unjsonPrettyParsec
--       <*> fieldBy "elabPkgOrComp/" elabPkgOrComp "elabPkgOrComp" unjsonPrettyParsec

data UnjsonDefAll = forall a. UnjsonDefAll a

unjsonSomething :: forall a. Typeable a => Proxy a -> UnjsonDefAll
unjsonSomething _proxy | Just Refl <- eqT @a @String = UnjsonDefAll (unjsonDef @String)

unjsonStructure :: forall a. Structured a => UnjsonDef a
unjsonStructure =
  case structure (Proxy :: Proxy a) of
    Nominal rep ver name strs -> _
    Newtype rep ver name str -> _
    Structure rep ver name sop -> _

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
