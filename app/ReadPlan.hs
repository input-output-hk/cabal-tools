{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Monad (unless)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor.Invariant (invmap)
import Data.Maybe (fromJust)
import Data.Traversable (for)
import Data.Tuple (swap)
import GHC.Generics
import System.IO

import Distribution.Backpack (DefUnitId, OpenModule, OpenUnitId)
import Distribution.Backpack.ModuleShape
import Distribution.Compat.Newtype
import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple.InstallDirs (PathTemplate, fromPathTemplate, toPathTemplate)
import Distribution.System
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Flag
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Utils.Structured hiding (typeName)

import Distribution.Client.FileMonitor
import Distribution.Client.HashValue
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.PackageHash
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types
import Distribution.Client.Types.SourceRepo

import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), OptionalStanzaMap, OptionalStanzaSet, optStanzaIndex, optStanzaSetFromList, optStanzaSetToList, optStanzaTabulate)

import Control.Applicative.Free (Ap)
import Data.Aeson as Aeson hiding (Key, Value)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.Binary
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Typeable (Typeable)
import Data.Unjson
import Distribution.Simple.Compiler (PackageDB)
import Distribution.Simple.Setup (readPackageDb, readPackageDbList, showPackageDb, showPackageDbList)
import Generic.Data (Constructors, GBounded, GDatatype, GEnum, StandardEnum, gconName, gdatatypeName, genumFromTo, gmaxBound, gminBound)

unjsonElaboratedConfiguredPackage :: UnjsonDef ElaboratedConfiguredPackage
unjsonElaboratedConfiguredPackage =
    objectOf $
        ElaboratedConfiguredPackage
            <$> fieldBy "elabUnitId/" elabUnitId "elabUnitId" unjsonPrettyParsec
            <*> fieldBy "elabComponentId/" elabComponentId "elabComponentId" unjsonPrettyParsec
            <*> fieldBy "elabInstantiatedWith/" elabInstantiatedWith "elabInstantiatedWith" (mapOfBy unjsonPrettyParsec unjsonPrettyParsec)
            <*> fieldBy "elabLinkedInstantiatedWith/" elabLinkedInstantiatedWith "elabLinkedInstantiatedWith" (mapOfBy unjsonPrettyParsec unjsonPrettyParsec)
            <*> fieldBy "elabIsCanonical/" elabIsCanonical "elabIsCanonical" unjsonPrettyParsec
            <*> fieldBy "elabPkgSourceId/" elabPkgSourceId "elabPkgSourceId" unjsonPrettyParsec
            <*> fieldBy "elabModuleShape/" elabModuleShape "elabModuleShape" unjsonModuleShape
            <*> fieldBy "elabFlagAssignment/" elabFlagAssignment "elabFlagAssignment" unjsonPrettyParsec
            <*> fieldBy "elabFlagDefaults/" elabFlagDefaults "elabFlagDefaults" unjsonPrettyParsec
            <*> fieldBy "elabPkgDescription/" elabPkgDescription "elabPkgDescription" unjsonPackageDescription
            <*> fieldBy "elabPkgSourceLocation/" elabPkgSourceLocation "elabPkgSourceLocation" unjsonPackageLocation
            <*> fieldOptBy "elabPkgSourceHash/" elabPkgSourceHash "elabPkgSourceHash" unjsonPkgSourceHash
            <*> fieldBy "elabLocalToProject/" elabLocalToProject "elabLocalToProject" unjsonPrettyParsec
            <*> fieldBy "elabBuildStyle/" elabBuildStyle "elabBuildStyle" unjsonGEnumOf
            <*> fieldBy "elabEnabledSpec/" elabEnabledSpec "elabEnabledSpec" unjsonShowRead
            <*> fieldBy "elabStanzasAvailable/" elabStanzasAvailable "elabStanzasAvailable" unjsonOptionalStanzaSet
            <*> fieldBy "elabStanzasRequested/" elabStanzasRequested "elabStanzasRequested" (unjsonGenericAeson "OptionalStanzaMap (Maybe Bool)" defaultOptions)
            <*> fieldBy "elabPackageDbs/" elabPackageDbs "elabPackageDbs" (arrayOf unjsonPackageDb)
            <*> fieldBy "elabSetupPackageDBStack/" elabSetupPackageDBStack "elabSetupPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldBy "elabBuildPackageDBStack/" elabBuildPackageDBStack "elabBuildPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldBy "elabRegisterPackageDBStack/" elabRegisterPackageDBStack "elabRegisterPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldBy "elabInplaceSetupPackageDBStack/" elabInplaceSetupPackageDBStack "elabInplaceSetupPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldBy "elabInplaceBuildPackageDBStack/" elabInplaceBuildPackageDBStack "elabInplaceBuildPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldBy "elabInplaceRegisterPackageDBStack/" elabInplaceRegisterPackageDBStack "elabInplaceRegisterPackageDBStack" _ -- unjsonPrettyParsec
            <*> fieldOptBy "elabPkgDescriptionOverride/" elabPkgDescriptionOverride "elabPkgDescriptionOverride" (coerce unjsonUTF8LazyByteString)
            <*> fieldBy "elabVanillaLib/" elabVanillaLib "elabVanillaLib" unjsonPrettyParsec
            <*> fieldBy "elabSharedLib/" elabSharedLib "elabSharedLib" unjsonPrettyParsec
            <*> fieldBy "elabStaticLib/" elabStaticLib "elabStaticLib" unjsonPrettyParsec
            <*> fieldBy "elabDynExe/" elabDynExe "elabDynExe" unjsonPrettyParsec
            <*> fieldBy "elabFullyStaticExe/" elabFullyStaticExe "elabFullyStaticExe" unjsonPrettyParsec
            <*> fieldBy "elabGHCiLib/" elabGHCiLib "elabGHCiLib" unjsonPrettyParsec
            <*> fieldBy "elabProfLib/" elabProfLib "elabProfLib" unjsonPrettyParsec
            <*> fieldBy "elabProfExe/" elabProfExe "elabProfExe" unjsonPrettyParsec
            <*> fieldBy "elabProfLibDetail/" elabProfLibDetail "elabProfLibDetail" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabProfExeDetail/" elabProfExeDetail "elabProfExeDetail" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabCoverage/" elabCoverage "elabCoverage" unjsonPrettyParsec
            <*> fieldBy "elabOptimization/" elabOptimization "elabOptimization" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabSplitObjs/" elabSplitObjs "elabSplitObjs" unjsonPrettyParsec
            <*> fieldBy "elabSplitSections/" elabSplitSections "elabSplitSections" unjsonPrettyParsec
            <*> fieldBy "elabStripLibs/" elabStripLibs "elabStripLibs" unjsonPrettyParsec
            <*> fieldBy "elabStripExes/" elabStripExes "elabStripExes" unjsonPrettyParsec
            <*> fieldBy "elabDebugInfo/" elabDebugInfo "elabDebugInfo" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabDumpBuildInfo/" elabDumpBuildInfo "elabDumpBuildInfo" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> field "elabProgramPaths/" elabProgramPaths "elabProgramPaths"
            <*> field "elabProgramArgs/" elabProgramArgs "elabProgramArgs"
            <*> fieldBy "elabProgramPathExtra/" elabProgramPathExtra "elabProgramPathExtra" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> field "elabConfigureScriptArgs/" elabConfigureScriptArgs "elabConfigureScriptArgs"
            <*> field "elabExtraLibDirs/" elabExtraLibDirs "elabExtraLibDirs"
            <*> field "elabExtraLibDirsStatic/" elabExtraLibDirsStatic "elabExtraLibDirsStatic"
            <*> field "elabExtraFrameworkDirs/" elabExtraFrameworkDirs "elabExtraFrameworkDirs"
            <*> field "elabExtraIncludeDirs/" elabExtraIncludeDirs "elabExtraIncludeDirs"
            <*> fieldOptBy "elabProgPrefix/" elabProgPrefix "elabProgPrefix" unjsonPathTemplate
            <*> fieldOptBy "elabProgSuffix/" elabProgSuffix "elabProgSuffix" unjsonPathTemplate
            <*> fieldBy "elabInstallDirs/" elabInstallDirs "elabInstallDirs" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabHaddockHoogle/" elabHaddockHoogle "elabHaddockHoogle" unjsonPrettyParsec
            <*> fieldBy "elabHaddockHtml/" elabHaddockHtml "elabHaddockHtml" unjsonPrettyParsec
            <*> fieldOpt "elabHaddockHtmlLocation/" elabHaddockHtmlLocation "elabHaddockHtmlLocation"
            <*> fieldBy "elabHaddockForeignLibs/" elabHaddockForeignLibs "elabHaddockForeignLibs" unjsonPrettyParsec
            <*> fieldBy "elabHaddockForHackage/" elabHaddockForHackage "elabHaddockForHackage" unjsonPrettyParsec
            <*> fieldBy "elabHaddockExecutables/" elabHaddockExecutables "elabHaddockExecutables" unjsonPrettyParsec
            <*> fieldBy "elabHaddockTestSuites/" elabHaddockTestSuites "elabHaddockTestSuites" unjsonPrettyParsec
            <*> fieldBy "elabHaddockBenchmarks/" elabHaddockBenchmarks "elabHaddockBenchmarks" unjsonPrettyParsec
            <*> fieldBy "elabHaddockInternal/" elabHaddockInternal "elabHaddockInternal" unjsonPrettyParsec
            <*> fieldOpt "elabHaddockCss/" elabHaddockCss "elabHaddockCss"
            <*> fieldBy "elabHaddockLinkedSource/" elabHaddockLinkedSource "elabHaddockLinkedSource" unjsonPrettyParsec
            <*> fieldBy "elabHaddockQuickJump/" elabHaddockQuickJump "elabHaddockQuickJump" unjsonPrettyParsec
            <*> fieldOpt "elabHaddockHscolourCss/" elabHaddockHscolourCss "elabHaddockHscolourCss"
            <*> fieldOptBy "elabHaddockContents/" elabHaddockContents "elabHaddockContents" unjsonPathTemplate
            <*> fieldOptBy "elabHaddockIndex/" elabHaddockIndex "elabHaddockIndex" unjsonPathTemplate
            <*> fieldOpt "elabHaddockBaseUrl/" elabHaddockBaseUrl "elabHaddockBaseUrl"
            <*> fieldOpt "elabHaddockLib/" elabHaddockLib "elabHaddockLib"
            <*> fieldOptBy "elabTestMachineLog/" elabTestMachineLog "elabTestMachineLog" unjsonPathTemplate
            <*> fieldOptBy "elabTestHumanLog/" elabTestHumanLog "elabTestHumanLog" unjsonPathTemplate
            <*> fieldOptBy "elabTestShowDetails/" elabTestShowDetails "elabTestShowDetails" unjsonPrettyParsec
            <*> fieldBy "elabTestKeepTix/" elabTestKeepTix "elabTestKeepTix" unjsonPrettyParsec
            <*> fieldOpt "elabTestWrapper/" elabTestWrapper "elabTestWrapper"
            <*> fieldBy "elabTestFailWhenNoTestSuites/" elabTestFailWhenNoTestSuites "elabTestFailWhenNoTestSuites" unjsonPrettyParsec
            <*> fieldBy "elabTestTestOptions/" elabTestTestOptions "elabTestTestOptions" (arrayOf unjsonPathTemplate)
            <*> fieldBy "elabBenchmarkOptions/" elabBenchmarkOptions "elabBenchmarkOptions" (arrayOf unjsonPathTemplate)
            <*> fieldBy "elabSetupScriptStyle/" elabSetupScriptStyle "elabSetupScriptStyle" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
            <*> fieldBy "elabSetupScriptCliVersion/" elabSetupScriptCliVersion "elabSetupScriptCliVersion" unjsonPrettyParsec
            <*> fieldBy "elabConfigureTargets/" elabConfigureTargets "elabConfigureTargets" (arrayOf unjsonComponentTarget)
            <*> fieldBy "elabBuildTargets/" elabBuildTargets "elabBuildTargets" (arrayOf unjsonComponentTarget)
            <*> fieldBy "elabTestTargets/" elabTestTargets "elabTestTargets" (arrayOf unjsonComponentTarget)
            <*> fieldBy "elabBenchTargets/" elabBenchTargets "elabBenchTargets" (arrayOf unjsonComponentTarget)
            <*> fieldOptBy "elabReplTarget/" elabReplTarget "elabReplTarget" unjsonComponentTarget
            <*> fieldBy "elabHaddockTargets/" elabHaddockTargets "elabHaddockTargets" (arrayOf unjsonComponentTarget)
            <*> fieldBy "elabBuildHaddocks/" elabBuildHaddocks "elabBuildHaddocks" unjsonPrettyParsec
            <*> fieldBy "elabPkgOrComp/" elabPkgOrComp "elabPkgOrComp" unjsonElaboratedPackageOrComponent

unjsonPackageDb :: UnjsonDef (Maybe PackageDB)
unjsonPackageDb =
    invmap readPackageDb showPackageDb unjsonDef

unjsonElaboratedPackageOrComponent :: UnjsonDef ElaboratedPackageOrComponent
unjsonElaboratedPackageOrComponent =
    disjointUnionOf
        "ElaboratedPackageOrComponent"
        [ ("ElabPackage", (== "ElabPackage") . gconName, ElabPackage <$> fieldBy "ElabPackage" projectElabPackage "ElabPackage" unjsonElaboratedPackage)
        , ("ElabComponent", (== "ElabComponent") . gconName, ElabComponent <$> fieldBy "ElabComponent" projectElabComponent "ElabComponent" unjsonElaboratedComponent)
        ]
  where
    projectElabPackage ~(ElabPackage elabPkg) = elabPkg
    projectElabComponent ~(ElabComponent elabComp) = elabComp

unjsonElaboratedComponent :: UnjsonDef ElaboratedComponent
unjsonElaboratedComponent =
    objectOf $
        ElaboratedComponent
            <$> fieldBy
                "compSolverName"
                compSolverName
                "The name of the component to be built according to the solver"
                _
            <*> fieldOptBy
                "compComponentName"
                compComponentName
                "The name of the component to be built. Nothing if it's a setup dep"
                unjsonPrettyParsec
            <*> fieldBy
                "compLibDependencies"
                compLibDependencies
                "The *external* library dependencies of this component. We pass this to the configure script."
                (arrayOf unjsonConfiguredId)
            <*> fieldBy
                "compLinkedLibDependencies"
                compLinkedLibDependencies
                "In a component prior to instantiation, this list specifies the 'OpenUnitId's which, after instantiation, are the actual dependencies of this package.  Note that this does NOT include signature packages, which do not turn into real ordering dependencies when we instantiate.  This is intended to be a purely temporary field, to carry some information to the instantiation phase. It's more precise than 'compLibDependencies', and also stores information about internal dependencies."
                (arrayOf unjsonPrettyParsec)
            <*> fieldBy
                "compExeDependencies"
                compExeDependencies
                "The executable dependencies of this component (including internal executables)."
                (arrayOf unjsonConfiguredId)
            <*> fieldBy
                "compPkgConfigDependencies"
                compPkgConfigDependencies
                "The @pkg-config@ dependencies of the component"
                (arrayOf (unjsonTuple2By unjsonPrettyParsec unjsonPrettyParsec))
            <*> fieldBy
                "compExeDependencyPaths"
                compExeDependencyPaths
                "The paths all our executable dependencies will be installed to once they are installed."
                (arrayOf (unjsonTuple2By unjsonConfiguredId unjsonDef))
            <*> fieldBy
                "compOrderLibDependencies"
                compOrderLibDependencies
                "The UnitIds of the libraries (identifying elaborated packages/ components) that must be built before this project.  This is used purely for ordering purposes.  It can contain both references to definite and indefinite packages; an indefinite UnitId indicates that we must typecheck that indefinite package before we can build this one."
                (arrayOf unjsonPrettyParsec)

unjsonConfiguredId :: UnjsonDef ConfiguredId
unjsonConfiguredId = _

unjsonElaboratedPackage :: UnjsonDef ElaboratedPackage
unjsonElaboratedPackage = _

unjsonComponentTarget :: UnjsonDef ComponentTarget
unjsonComponentTarget =
    invmap (`ComponentTarget` WholeComponent) (\(ComponentTarget cn _) -> cn) unjsonPrettyParsec

class MyUnjson a where
    myUnjson :: UnjsonDef a

instance (Parsec a, Pretty a) => MyUnjson a where
    myUnjson = unjsonPrettyParsec

unjsonPackageDescription :: UnjsonDef PackageDescription
unjsonPackageDescription = _

hackyToHashValue :: String -> Either String HashValue
hackyToHashValue value =
    -- NOTE: this is a hack!
    -- HashValue does not expose its constructor but it does expose a Generic instance,
    -- so we can create an HashValue from the knowledge of its representation
    to . M1 . M1 . M1 . K1 <$> Base16.decode (C8.pack value)

unjsonPkgSourceHash :: UnjsonDef HashValue
unjsonPkgSourceHash =
    unjsonInvmapR
        (either fail return . hackyToHashValue)
        showHashValue
        unjsonDef

-- unjsonStructured :: forall a. (Structured a) => UnjsonDef a
-- unjsonStructured =
--     case structure (Proxy :: Proxy a) of
--         Nominal tyRep tyVer tyName tyTags ->
--             unjsonStructuredNominal tyRep tyVer tyName tyTags
--         Newtype tyRep tyVer tyName ty ->
--             unjsonStructuredNewType tyRep tyVer tyName ty
--         Structure tyRep tyVer tyName tySop ->
--             unjsonStructuredStructure tyRep tyVer tyName tySop
--
-- unjsonStructuredStructure
--     :: SomeTypeRep
--     -> TypeVersion
--     -> TypeName
--     -> SopStructure
--     -> UnjsonDef a
-- unjsonStructuredStructure = _
--
-- unjsonStructuredNewType
--     :: SomeTypeRep
--     -> TypeVersion
--     -> TypeName
--     -> Structure
--     -> UnjsonDef a
-- unjsonStructuredNewType = _
--
-- unjsonStructuredNominal
--     :: SomeTypeRep
--     -> TypeVersion
--     -> TypeName
--     -> [Structure]
--     -> UnjsonDef a
-- unjsonStructuredNominal = _

unjsonUTF8ByteString :: UnjsonDef BS.ByteString
unjsonUTF8ByteString =
    invmap T.encodeUtf8 T.decodeUtf8 unjsonDef

unjsonUTF8LazyByteString :: UnjsonDef BL.ByteString
unjsonUTF8LazyByteString =
    invmap TL.encodeUtf8 TL.decodeUtf8 unjsonDef

unjsonGenericAeson :: (Generic a, GFromJSON Zero (Rep a), GToJSON' Aeson.Value Zero (Rep a)) => T.Text -> Aeson.Options -> UnjsonDef a
unjsonGenericAeson docstring options =
    SimpleUnjsonDef
        docstring
        (either fail return . Aeson.parseEither (Aeson.genericParseJSON options))
        (Aeson.genericToJSON options)

unjsonOptionalStanzaSet :: UnjsonDef OptionalStanzaSet
unjsonOptionalStanzaSet =
    invmap optStanzaSetFromList optStanzaSetToList (arrayOf unjsonOptionalStanza)

-- | Relying on optStanzaTabulate . optStanzaIndex === id
unjsonOptionalStanzaMap :: (Eq a, Typeable a) => UnjsonDef a -> UnjsonDef (OptionalStanzaMap a)
unjsonOptionalStanzaMap def =
    invmap optStanzaTabulate optStanzaIndex (unjsonTotalMap def)

unjsonOptionalStanza :: UnjsonDef OptionalStanza
unjsonOptionalStanza = unjsonGEnumOf

unjsonPackageLocation :: UnjsonDef (PackageLocation local)
unjsonPackageLocation = _ -- unjsonGenericAeson [] "PackageLocation" defaultOptions

unjsonModuleShape :: UnjsonDef ModuleShape
unjsonModuleShape =
    objectOf $
        ModuleShape
            <$> fieldBy "modShapeProvides" modShapeProvides "modShapeProvides" (mapOfBy unjsonPrettyParsec unjsonPrettyParsec)
            <*> fieldBy "modShapeRequires" modShapeRequires "modShapeRequires" (invmap Set.fromList Set.toList (arrayOf unjsonPrettyParsec))

unjsonPathTemplate :: UnjsonDef PathTemplate
unjsonPathTemplate =
    invmap toPathTemplate fromPathTemplate unjsonDef

unjsonPrettyParsec :: (Parsec a, Pretty a) => UnjsonDef a
unjsonPrettyParsec =
    unjsonInvmapR
        (either fail return . eitherParsec)
        prettyShow
        unjsonDef

unjsonShowRead :: (Show a, Read a) => UnjsonDef a
unjsonShowRead = invmap read show unjsonDef

unjsonGEnumOf :: forall a. (Eq a, Constructors a, GEnum StandardEnum (Rep a), GBounded (Rep a), GDatatype (Rep a)) => UnjsonDef a
unjsonGEnumOf =
    enumOf
        (T.pack $ gdatatypeName @a)
        [(T.pack $ gconName bs, bs) | bs <- genumFromTo gminBound gmaxBound]

type EnumerableConstructors a = (Constructors a, GEnum StandardEnum (Rep a), GBounded (Rep a))

unjsonTotalMap :: forall k a. (Typeable a, Eq k, Typeable k, EnumerableConstructors k) => UnjsonDef a -> UnjsonDef (k -> a)
unjsonTotalMap def =
    objectOf $ unsafeLookup <$> for allValues mkField
  where
    unsafeLookup l v =
        fromJust (error "this should have never happened of course") $
            Prelude.lookup v l
    mkField v =
        fieldBy
            (T.pack $ gconName v)
            (\f -> (v, f v))
            (T.pack $ gconName v)
            (invmap (v,) snd def)

allValues :: (Generic a, GEnum StandardEnum (Rep a), GBounded (Rep a)) => [a]
allValues = genumFromTo gminBound gmaxBound

-- | This is encoded as a list of pairs
mapOfBy :: forall k v. (Ord k, Typeable k, Typeable v) => UnjsonDef k -> UnjsonDef v -> UnjsonDef (Map k v)
mapOfBy uk uv =
    invmap M.fromList M.toList $ arrayWithPrimaryKeyOf fst uk (unjsonTuple2By uk uv)

-- | Workaround because TupleFieldDef is not exposed
unjsonTuple2By :: UnjsonDef k -> UnjsonDef v -> UnjsonDef (k, v)
unjsonTuple2By uk uv =
    unjsonInvmapR
        (\(vk, vv) -> (,) <$> parse uk vk <*> parse uv vv)
        (bimap (unjsonToJSON uk) (unjsonToJSON uv))
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

withCacheFile
    :: (Binary a, Structured a, Binary b, Structured b)
    => FilePath
    -> (Either String (MonitorStateFileSet, a, Either String b) -> IO r)
    -> IO r
withCacheFile cacheFile k =
    withBinaryFile cacheFile ReadMode $ \hnd -> do
        contents <- structuredDecodeTriple <$> BL.hGetContents hnd
        k contents

structuredDecodeTriple
    :: forall a b c
     . (Structured a, Structured b, Structured c, Binary a, Binary b, Binary c)
    => BL.ByteString
    -> Either String (a, b, Either String c)
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
