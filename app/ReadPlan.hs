{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Applicative ((<|>))
import Control.Applicative.Free (Ap, hoistAp)
import Control.Monad (unless)
import Data.Aeson hiding (Result)
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
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Functor.Invariant (invmap)
import Data.Kind
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.Unjson
import Distribution.Backpack
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.ModuleShape
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
import Distribution.Compat.Newtype
import Distribution.ModuleName
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Simple
import Distribution.Simple.Compiler (PackageDB)
import Distribution.Simple.InstallDirs (PathTemplate, fromPathTemplate, toPathTemplate)
import Distribution.Simple.Setup (HaddockTarget, TestShowDetails, readPackageDb, readPackageDbList, showPackageDb, showPackageDbList)
import Distribution.Solver.Types.ComponentDeps qualified as CD
import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..), OptionalStanzaMap, OptionalStanzaSet, optStanzaIndex, optStanzaSetFromList, optStanzaSetToList, optStanzaTabulate)
import Distribution.System
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Flag
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.SourceRepo
import Distribution.Types.UnitId
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Utils.Structured hiding (typeName)
import GHC.Generics
import GHC.TypeLits
import Generic.Data (Constructors, GBounded, GDatatype, GEnum, GenericProduct (..), MetaConsRecord, MetaOf, StandardEnum, gconName, gdatatypeName, genumFromTo, gmaxBound, gminBound)
import Generic.Data.Internal.Meta (GDatatype (..))
import Lens.Micro hiding (to)
import Network.URI (URI, parseURI)
import System.IO
import Text.Read (readMaybe)

-- Note: contramapFieldDef and contramapTupleFieldDef are basically
-- Contravariant, but due to type parameters in wrong order we would
-- need to do some type shuffling to get it right. Easier to just
-- write it here as it is.
contramapFieldDef :: (b -> a) -> FieldDef a x -> FieldDef b x
contramapFieldDef f (FieldReqDef name doc ext d) = FieldReqDef name doc (ext . f) d
contramapFieldDef f (FieldOptDef name doc ext d) = FieldOptDef name doc (ext . f) d
contramapFieldDef f (FieldDefDef name doc def ext d) = FieldDefDef name doc def (ext . f) d
contramapFieldDef f (FieldRODef name doc ext d) = FieldRODef name doc (ext . f) d

-- class ToField' s f where
--   toField' :: f p -> (s -> a) -> Ap (FieldDef s) a

-- data    V1        p                       -- lifted version of Empty
-- data    U1        p = U1                  -- lifted version of ()
-- data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
-- data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
-- newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
-- newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-- FieldDef s a
--  s structure
--  a element

class FieldDef' a where
  fieldDef' :: Ap (FieldDef a) a
  default fieldDef' :: (Generic a, GFieldDef' (Rep a)) => Ap (FieldDef a) a
  fieldDef' = to <$> hoistAp (contramapFieldDef from) gFieldDef'

class GFieldDef' ra where
  gFieldDef' :: Ap (FieldDef (ra x)) (ra x)

instance (GFieldDef' ra) => GFieldDef' (D1 t ra) where
  gFieldDef' = M1 <$> hoistAp (contramapFieldDef unM1) gFieldDef'

instance (GFieldDef' ra) => GFieldDef' (C1 t ra) where
  gFieldDef' = M1 <$> hoistAp (contramapFieldDef unM1) gFieldDef'

instance (GFieldDef' ra) => GFieldDef' (S1 t ra) where
  gFieldDef' = M1 <$> hoistAp (contramapFieldDef unM1) gFieldDef'

instance (Typeable c, Unjson c) => GFieldDef' (K1 i c) where
  gFieldDef' = K1 <$> hoistAp (contramapFieldDef unK1) (field "" id "")

instance (GFieldDef' l, GFieldDef' r) => GFieldDef' (l :*: r) where
  gFieldDef' :: Ap (FieldDef ((:*:) l r x)) ((:*:) l r x)
  gFieldDef' =
    let l = gFieldDef' @l
        r = gFieldDef' @r
        l' = hoistAp (contramapFieldDef fst') l
        r' = hoistAp (contramapFieldDef snd') r
     in liftA2 (:*:) l' r'

-- where
--   name = T.pack $ symbolVal (Proxy :: Proxy n)

-- gFieldDef' :: forall pr. Ap (FieldDef ((:*:) l r pl)) ((:*:) l r pr)
-- gFieldDef' =
--   let l = gFieldDef' @l
--       r = gFieldDef' @r
--       l' = hoistAp (contramapFieldDef fst') l :: Ap (FieldDef ((:*:) l r p)) a
--       r' = hoistAp (contramapFieldDef snd') r :: Ap (FieldDef ((:*:) l r p)) a
--    in _

fst' :: (f :*: g) p -> f p
fst' (f :*: _) = f

snd' :: (f :*: g) p -> g p
snd' (_ :*: g) = g

newtype A = A {unA :: Int} deriving (Generic)

instance FieldDef' A

instance FieldDef' PackageId

-- class GExtractSelector (f :: Type -> Type) (s :: Type) (a :: Type) where
--   gExtractSelector :: Proxy f -> Ap (FieldDef s) a
--
-- instance (Unjson a, Typeable a) => GExtractSelector (f :*: g) s a where
--   gExtractSelector = _
--
-- instance (Unjson a, Typeable a, KnownSymbol nm, Rep s ~ f) => GExtractSelector (M1 S (MetaSel (Just nm) su ss ds) (K1 i a)) s a where
--   gExtractSelector proxy = field name (unK1 . unM1) name
--     where
--       name = T.pack $ symbolVal (Proxy :: Proxy nm)

-- test :: Ap (FieldDef ConfiguredId) ConfiguredId
-- test = toFieldCons' (Proxy @ConfiguredId) (Proxy @ConfiguredId)

-- test :: Ap (FieldDef PackageName) PackageName
-- test = toFieldCons' Proxy Proxy Proxy from

-- class ToField s where
--   toField :: s -> Ap (FieldDef s) s
--   default toField :: (Generic s, ToField' s (Rep s)) => s -> Ap (FieldDef s) s
--   toField x = toField' (from x)

unjsonElaboratedConfiguredPackage :: UnjsonDef ElaboratedConfiguredPackage
unjsonElaboratedConfiguredPackage =
  objectOf $
    ElaboratedConfiguredPackage
      <$> field "elabUnitId/" elabUnitId "elabUnitId"
      <*> field "elabComponentId/" elabComponentId "elabComponentId"
      <*> fieldBy "elabInstantiatedWith/" elabInstantiatedWith "elabInstantiatedWith" (mapOfBy unjsonDef unjsonDef)
      <*> fieldBy "elabLinkedInstantiatedWith/" elabLinkedInstantiatedWith "elabLinkedInstantiatedWith" (mapOfBy unjsonDef unjsonDef)
      <*> field "elabIsCanonical/" elabIsCanonical "elabIsCanonical"
      <*> field "elabPkgSourceId/" elabPkgSourceId "elabPkgSourceId"
      <*> fieldBy "elabModuleShape/" elabModuleShape "elabModuleShape" unjsonModuleShape
      <*> field "elabFlagAssignment/" elabFlagAssignment "elabFlagAssignment"
      <*> field "elabFlagDefaults/" elabFlagDefaults "elabFlagDefaults"
      <*> fieldBy "elabPkgDescription/" elabPkgDescription "elabPkgDescription" unjsonPackageDescription
      <*> fieldBy "elabPkgSourceLocation/" elabPkgSourceLocation "elabPkgSourceLocation" unjsonPackageLocation
      <*> fieldOpt "elabPkgSourceHash/" elabPkgSourceHash "elabPkgSourceHash"
      <*> field "elabLocalToProject/" elabLocalToProject "elabLocalToProject"
      <*> field "elabBuildStyle/" elabBuildStyle "elabBuildStyle"
      <*> field "elabEnabledSpec/" elabEnabledSpec "elabEnabledSpec"
      <*> field "elabStanzasAvailable/" elabStanzasAvailable "elabStanzasAvailable"
      <*> field "elabStanzasRequested/" elabStanzasRequested "elabStanzasRequested"
      <*> fieldBy "elabPackageDbs/" elabPackageDbs "elabPackageDbs" (arrayOf unjsonPackageDb)
      <*> field "elabSetupPackageDBStack/" elabSetupPackageDBStack "elabSetupPackageDBStack"
      <*> field "elabBuildPackageDBStack/" elabBuildPackageDBStack "elabBuildPackageDBStack"
      <*> field "elabRegisterPackageDBStack/" elabRegisterPackageDBStack "elabRegisterPackageDBStack"
      <*> field "elabInplaceSetupPackageDBStack/" elabInplaceSetupPackageDBStack "elabInplaceSetupPackageDBStack"
      <*> field "elabInplaceBuildPackageDBStack/" elabInplaceBuildPackageDBStack "elabInplaceBuildPackageDBStack"
      <*> field "elabInplaceRegisterPackageDBStack/" elabInplaceRegisterPackageDBStack "elabInplaceRegisterPackageDBStack"
      <*> fieldOpt "elabPkgDescriptionOverride/" elabPkgDescriptionOverride "elabPkgDescriptionOverride"
      <*> field "elabVanillaLib/" elabVanillaLib "elabVanillaLib"
      <*> field "elabSharedLib/" elabSharedLib "elabSharedLib"
      <*> field "elabStaticLib/" elabStaticLib "elabStaticLib"
      <*> field "elabDynExe/" elabDynExe "elabDynExe"
      <*> field "elabFullyStaticExe/" elabFullyStaticExe "elabFullyStaticExe"
      <*> field "elabGHCiLib/" elabGHCiLib "elabGHCiLib"
      <*> field "elabProfLib/" elabProfLib "elabProfLib"
      <*> field "elabProfExe/" elabProfExe "elabProfExe"
      <*> field "elabProfLibDetail/" elabProfLibDetail "elabProfLibDetail"
      <*> fieldBy "elabProfExeDetail/" elabProfExeDetail "elabProfExeDetail" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
      <*> field "elabCoverage/" elabCoverage "elabCoverage"
      <*> fieldBy "elabOptimization/" elabOptimization "elabOptimization" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
      <*> field "elabSplitObjs/" elabSplitObjs "elabSplitObjs"
      <*> field "elabSplitSections/" elabSplitSections "elabSplitSections"
      <*> field "elabStripLibs/" elabStripLibs "elabStripLibs"
      <*> field "elabStripExes/" elabStripExes "elabStripExes"
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
      <*> fieldOpt "elabProgPrefix/" elabProgPrefix "elabProgPrefix"
      <*> fieldOpt "elabProgSuffix/" elabProgSuffix "elabProgSuffix"
      <*> fieldBy "elabInstallDirs/" elabInstallDirs "elabInstallDirs" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
      <*> field "elabHaddockHoogle/" elabHaddockHoogle "elabHaddockHoogle"
      <*> field "elabHaddockHtml/" elabHaddockHtml "elabHaddockHtml"
      <*> fieldOpt "elabHaddockHtmlLocation/" elabHaddockHtmlLocation "elabHaddockHtmlLocation"
      <*> field "elabHaddockForeignLibs/" elabHaddockForeignLibs "elabHaddockForeignLibs"
      <*> field "elabHaddockForHackage/" elabHaddockForHackage "elabHaddockForHackage"
      <*> field "elabHaddockExecutables/" elabHaddockExecutables "elabHaddockExecutables"
      <*> field "elabHaddockTestSuites/" elabHaddockTestSuites "elabHaddockTestSuites"
      <*> field "elabHaddockBenchmarks/" elabHaddockBenchmarks "elabHaddockBenchmarks"
      <*> field "elabHaddockInternal/" elabHaddockInternal "elabHaddockInternal"
      <*> fieldOpt "elabHaddockCss/" elabHaddockCss "elabHaddockCss"
      <*> field "elabHaddockLinkedSource/" elabHaddockLinkedSource "elabHaddockLinkedSource"
      <*> field "elabHaddockQuickJump/" elabHaddockQuickJump "elabHaddockQuickJump"
      <*> fieldOpt "elabHaddockHscolourCss/" elabHaddockHscolourCss "elabHaddockHscolourCss"
      <*> fieldOpt "elabHaddockContents/" elabHaddockContents "elabHaddockContents"
      <*> fieldOpt "elabHaddockIndex/" elabHaddockIndex "elabHaddockIndex"
      <*> fieldOpt "elabHaddockBaseUrl/" elabHaddockBaseUrl "elabHaddockBaseUrl"
      <*> fieldOpt "elabHaddockLib/" elabHaddockLib "elabHaddockLib"
      <*> fieldOpt "elabTestMachineLog/" elabTestMachineLog "elabTestMachineLog"
      <*> fieldOpt "elabTestHumanLog/" elabTestHumanLog "elabTestHumanLog"
      <*> fieldOpt "elabTestShowDetails/" elabTestShowDetails "elabTestShowDetails"
      <*> field "elabTestKeepTix/" elabTestKeepTix "elabTestKeepTix"
      <*> fieldOpt "elabTestWrapper/" elabTestWrapper "elabTestWrapper"
      <*> field "elabTestFailWhenNoTestSuites/" elabTestFailWhenNoTestSuites "elabTestFailWhenNoTestSuites"
      <*> field "elabTestTestOptions/" elabTestTestOptions "elabTestTestOptions"
      <*> field "elabBenchmarkOptions/" elabBenchmarkOptions "elabBenchmarkOptions"
      <*> fieldBy "elabSetupScriptStyle/" elabSetupScriptStyle "elabSetupScriptStyle" (unjsonGenericAeson "elabSetupScriptStyle" Aeson.defaultOptions)
      <*> field "elabSetupScriptCliVersion/" elabSetupScriptCliVersion "elabSetupScriptCliVersion"
      <*> field "elabConfigureTargets/" elabConfigureTargets "elabConfigureTargets"
      <*> field "elabBuildTargets/" elabBuildTargets "elabBuildTargets"
      <*> field "elabTestTargets/" elabTestTargets "elabTestTargets"
      <*> field "elabBenchTargets/" elabBenchTargets "elabBenchTargets"
      <*> fieldOpt "elabReplTarget/" elabReplTarget "elabReplTarget"
      <*> field "elabHaddockTargets/" elabHaddockTargets "elabHaddockTargets"
      <*> field "elabBuildHaddocks/" elabBuildHaddocks "elabBuildHaddocks"
      <*> fieldBy "elabPkgOrComp/" elabPkgOrComp "elabPkgOrComp" unjsonElaboratedPackageOrComponent

unjsonModuleShape :: UnjsonDef ModuleShape
unjsonModuleShape =
  objectOf $
    ModuleShape
      <$> fieldBy "modShapeProvides" modShapeProvides "modShapeProvides" (mapOfBy unjsonDef unjsonDef)
      <*> field "modShapeRequires" modShapeRequires "modShapeRequires"

unjsonPackageDb :: UnjsonDef (Maybe PackageDB)
unjsonPackageDb =
  invmap readPackageDb showPackageDb unjsonDef

unjsonElaboratedPackageOrComponent :: UnjsonDef ElaboratedPackageOrComponent
unjsonElaboratedPackageOrComponent =
  disjointUnionOf
    "ElaboratedPackageOrComponent"
    [ ( "ElabPackage",
        (== "ElabPackage") . gconName,
        ElabPackage
          <$> fieldBy "ElabPackage" projectElabPackage "ElabPackage" unjsonElaboratedPackage
      ),
      ( "ElabComponent",
        (== "ElabComponent") . gconName,
        ElabComponent
          <$> fieldBy "ElabComponent" projectElabComponent "ElabComponent" unjsonElaboratedComponent
      )
    ]
  where
    projectElabPackage ~(ElabPackage elabPkg) = elabPkg
    projectElabComponent ~(ElabComponent elabComp) = elabComp

unjsonElaboratedComponent :: UnjsonDef ElaboratedComponent
unjsonElaboratedComponent =
  objectOf $
    ElaboratedComponent
      <$> field
        "compSolverName"
        compSolverName
        "The name of the component to be built according to the solver"
      <*> fieldOpt
        "compComponentName"
        compComponentName
        "The name of the component to be built. Nothing if it's a setup dep"
      <*> field
        "compLibDependencies"
        compLibDependencies
        "The *external* library dependencies of this component. We pass this to the configure script."
      <*> field
        "compLinkedLibDependencies"
        compLinkedLibDependencies
        "In a component prior to instantiation, this list specifies the 'OpenUnitId's which, after instantiation, are the actual dependencies of this package.  Note that this does NOT include signature packages, which do not turn into real ordering dependencies when we instantiate.  This is intended to be a purely temporary field, to carry some information to the instantiation phase. It's more precise than 'compLibDependencies', and also stores information about internal dependencies."
      <*> field
        "compExeDependencies"
        compExeDependencies
        "The executable dependencies of this component (including internal executables)."
      <*> fieldBy
        "compPkgConfigDependencies"
        compPkgConfigDependencies
        "The @pkg-config@ dependencies of the component"
        (arrayOf unjsonPkgconfig) -- (unjsonTuple2By unjsonPrettyParsec unjsonPrettyParsec))
      <*> fieldBy
        "compExeDependencyPaths"
        compExeDependencyPaths
        "The paths all our executable dependencies will be installed to once they are installed."
        (arrayOf (unjsonTuple2By unjsonDef unjsonDef))
      <*> field
        "compOrderLibDependencies"
        compOrderLibDependencies
        "The UnitIds of the libraries (identifying elaborated packages/ components) that must be built before this project.  This is used purely for ordering purposes.  It can contain both references to definite and indefinite packages; an indefinite UnitId indicates that we must typecheck that indefinite package before we can build this one."

unjsonPkgconfig :: UnjsonDef (PkgconfigName, Maybe PkgconfigVersion)
unjsonPkgconfig =
  objectOf $ (,) <$> field "name" fst "pkg-config name" <*> fieldOpt "name" snd "pkg-config version"

unjsonPackageLocation :: UnjsonDef (PackageLocation (Maybe FilePath))
unjsonPackageLocation =
  disjointUnionOf
    "PackageLocation"
    [ ( "LocalUnpackedPackage",
        (== "LocalUnpackedPackage") . gconName,
        LocalUnpackedPackage
          <$> field
            "path"
            (\case ~(LocalUnpackedPackage fp) -> fp)
            "path"
      ),
      ( "LocalTarballPackage",
        (== "LocalTarballPackage") . gconName,
        LocalTarballPackage
          <$> field
            "path"
            (\case ~(LocalTarballPackage fp) -> fp)
            "path"
      ),
      ( "RemoteTarballPackage",
        (== "RemoteTarballPackage") . gconName,
        RemoteTarballPackage
          <$> field
            "uri"
            (\case ~(RemoteTarballPackage uri _fp) -> uri)
            "uri"
          <*> fieldOpt
            "path"
            (\case ~(RemoteTarballPackage _uri fp) -> fp)
            "path"
      ),
      ( "RepoTarballPackage",
        (== "RepoTarballPackage") . gconName,
        RepoTarballPackage
          <$> fieldBy
            "repo"
            (\case ~(RepoTarballPackage repo _pkg _fp) -> repo)
            "repo"
            unjsonRepo
          <*> field
            "packageId"
            (\case ~(RepoTarballPackage _repo pkg _fp) -> pkg)
            "packageId"
          <*> fieldOpt
            "path"
            (\case ~(RepoTarballPackage _repo _pkg fp) -> fp)
            "path"
      ),
      ( "RemoteSourceRepoPackage",
        (== "RemoteSourceRepoPackage") . gconName,
        RemoteSourceRepoPackage
          <$> field
            "srp"
            (\case ~(RemoteSourceRepoPackage srp _fp) -> srp)
            "srp"
          <*> fieldOpt
            "path"
            (\case ~(RemoteSourceRepoPackage _uri fp) -> fp)
            "path"
      )
    ]

unjsonRepo :: UnjsonDef Repo
unjsonRepo =
  disjointUnionOf
    "Repo"
    [ ( "RepoLocalNoIndex",
        (== "RepoLocalNoIndex") . gconName,
        RepoLocalNoIndex
          <$> field
            "repoLocal"
            (\case ~(RepoLocalNoIndex repo _fp) -> repo)
            "repoLocal"
          <*> field
            "repoLocalDir"
            (\case ~(RepoLocalNoIndex _repo fp) -> fp)
            "repoLocalDir"
      ),
      ( "RepoRemote",
        (== "RepoRemote") . gconName,
        RepoRemote
          <$> field
            "repoRemote"
            (\case ~(RepoRemote repo _fp) -> repo)
            "repoRemote"
          <*> field
            "repoLocalDir"
            (\case ~(RepoRemote _repo fp) -> fp)
            "repoLocalDir"
      ),
      ( "RepoSecure",
        (== "RepoSecure") . gconName,
        RepoSecure
          <$> field
            "repoSecure"
            (\case ~(RepoSecure repo _fp) -> repo)
            "repoSecure"
          <*> field
            "repoLocalDir"
            (\case ~(RepoSecure _repo fp) -> fp)
            "repoLocalDir"
      )
    ]

--
--
--

newtype UnjsonPrettyParsec a = UnjsonPrettyParsec a

deriving via (a :: Type) instance (Pretty a) => Pretty (UnjsonPrettyParsec a)

-- | NOTE: This does not work
-- deriving via a instance (Parsec a) => Parsec (UnjsonPrettyParsec a)
instance (Parsec a) => Parsec (UnjsonPrettyParsec a) where
  parsec = UnjsonPrettyParsec <$> parsec

instance (Pretty a, Parsec a) => Unjson (UnjsonPrettyParsec a) where
  unjsonDef = unjsonInvmapR (either fail return . eitherParsec) prettyShow unjsonDef

newtype UnjsonShowRead a = UnjsonShowRead a

deriving via (a :: Type) instance (Show a) => Show (UnjsonShowRead a)

deriving via (a :: Type) instance (Read a) => Read (UnjsonShowRead a)

instance (Show a, Read a) => Unjson (UnjsonShowRead a) where
  unjsonDef = unjsonInvmapR (maybe (fail "cannot parse") return . readMaybe) show unjsonDef

deriving via UnjsonShowRead (Maybe Bool) instance Unjson (Maybe Bool)

deriving via UnjsonPrettyParsec ComponentId instance Unjson ComponentId

deriving via UnjsonPrettyParsec ComponentName instance Unjson ComponentName

deriving via UnjsonShowRead ComponentRequestedSpec instance Unjson ComponentRequestedSpec

deriving via UnjsonPrettyParsec FlagAssignment instance Unjson FlagAssignment

deriving via UnjsonPrettyParsec HaddockTarget instance Unjson HaddockTarget

deriving via UnjsonPrettyParsec LocalRepo instance Unjson LocalRepo

deriving via UnjsonPrettyParsec Module instance Unjson Module

deriving via UnjsonPrettyParsec ModuleName instance Unjson ModuleName

deriving via UnjsonPrettyParsec OpenModule instance Unjson OpenModule

deriving via UnjsonPrettyParsec OpenUnitId instance Unjson OpenUnitId

deriving via UnjsonPrettyParsec PackageName instance Unjson PackageName

deriving via UnjsonPrettyParsec PackageId instance Unjson PackageId

deriving via UnjsonShowRead PackageDB instance Unjson PackageDB

deriving via UnjsonPrettyParsec PkgconfigName instance Unjson PkgconfigName

deriving via UnjsonPrettyParsec PkgconfigVersion instance Unjson PkgconfigVersion

deriving via UnjsonPrettyParsec RemoteRepo instance Unjson RemoteRepo

deriving via UnjsonPrettyParsec RepoType instance Unjson RepoType

deriving via UnjsonPrettyParsec TestShowDetails instance Unjson TestShowDetails

deriving via UnjsonPrettyParsec UnitId instance Unjson UnitId

deriving via UnjsonPrettyParsec Version instance Unjson Version

instance Unjson OptionalStanzaSet where
  unjsonDef = invmap optStanzaSetFromList optStanzaSetToList unjsonDef

deriving via UnjsonEnumeration OptionalStanza instance Unjson OptionalStanza

instance Unjson (OptionalStanzaMap (Maybe Bool)) where
  unjsonDef = invmap optStanzaTabulate optStanzaIndex unjsonTotalMap

instance Unjson CD.Component where
  unjsonDef = unjsonInvmapR parse' prettyShow (unjsonDef @String)
    where
      parse' :: String -> Result CD.Component
      parse' =
        either fail return
          . explicitEitherParsec
            ( CD.componentNameToComponent <$> parsec
                <|> error "fixme, missing setup"
            )

instance Unjson PathTemplate where
  unjsonDef = invmap toPathTemplate fromPathTemplate unjsonDef

instance Unjson ProfDetailLevel where
  unjsonDef = unjsonGenericAeson "ProfDetailLevel" Aeson.defaultOptions

instance Unjson HashValue where
  unjsonDef =
    unjsonInvmapR
      (either fail return . hackyToHashValue)
      showHashValue
      unjsonDef
    where
      hackyToHashValue :: String -> Either String HashValue
      hackyToHashValue value =
        -- NOTE: this is a hack!
        -- HashValue does not expose its constructor but it does expose a Generic instance,
        -- so we can create an HashValue from the knowledge of its representation
        GHC.Generics.to . M1 . M1 . M1 . K1 <$> Base16.decode (C8.pack value)

instance Unjson ConfiguredId where
  unjsonDef =
    objectOf $
      ConfiguredId
        <$> field "confSrcId" confSrcId "confSrcId"
        <*> fieldOpt "confCompName" confCompName "confCompName"
        <*> field "confInstId" confInstId "confInstId"

instance Unjson (SourceRepositoryPackage Maybe) where
  unjsonDef =
    objectOf $
      SourceRepositoryPackage
        <$> field "srpType" srpType "srpType"
        <*> field "srpLocation" srpLocation "srpLocation"
        <*> fieldOpt "srpTag" srpTag "srpTag"
        <*> fieldOpt "srpBranch" srpBranch "srpBranch"
        <*> fieldOpt "srpSubdir" srpSubdir "srpSubdir"
        <*> field "srpCommand" srpCommand "srpCommand"

unjsonElaboratedPackage :: UnjsonDef ElaboratedPackage
unjsonElaboratedPackage = undefined

-- | TODO: Fixing SubComponentTarget = WholeComponent for the moment
instance Unjson ComponentTarget where
  unjsonDef =
    invmap
      (`ComponentTarget` WholeComponent)
      (\(ComponentTarget cn _) -> cn)
      unjsonDef

unjsonPackageDescription :: UnjsonDef PackageDescription
unjsonPackageDescription = undefined

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

instance Unjson URI where
  unjsonDef =
    unjsonInvmapR
      (maybe (fail "cannot parse URI") pure . parseURI)
      show
      (unjsonDef @String)

instance Unjson BS.ByteString where
  unjsonDef = invmap T.encodeUtf8 T.decodeUtf8 unjsonDef

instance Unjson BL.ByteString where
  unjsonDef = invmap TL.encodeUtf8 TL.decodeUtf8 unjsonDef

type GFromToJSON a = (GFromJSON Zero a, GToJSON' Value Zero a)

unjsonGenericAeson :: (Generic a, GFromToJSON (Rep a)) => Text -> Aeson.Options -> UnjsonDef a
unjsonGenericAeson docstring options =
  SimpleUnjsonDef
    docstring
    (either fail return . Aeson.parseEither (Aeson.genericParseJSON options))
    (Aeson.genericToJSON options)

-- data    V1        p                       -- lifted version of Empty
-- data    U1        p = U1                  -- lifted version of ()
-- data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
-- data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
-- newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
-- newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-- data ThingyResult = ThingyResult
--
-- class Thingy' f where
--   thingy' :: f p -> ThingyResult
--
-- instance Thingy' V1 where thingy' x = case x of {}
--
-- instance Thingy' U1 where thingy' _ = ThingyResult
--
-- instance Thingy'

newtype UnjsonEnumeration a = UnjsonEnumeration a

instance
  ( Eq a,
    Constructors a,
    GEnum StandardEnum (Rep a),
    GBounded (Rep a),
    GDatatype (Rep a)
  ) =>
  Unjson (UnjsonEnumeration a)
  where
  unjsonDef =
    invmap coerce coerce $
      enumOf @a
        (T.pack $ gdatatypeName @a)
        [(T.pack $ gconName bs, bs) | bs <- genumFromTo gminBound gmaxBound]

deriving via UnjsonEnumeration BuildStyle instance Unjson BuildStyle

unjsonTotalMap :: (Eq k, Constructors k, GEnum StandardEnum (Rep k), GBounded (Rep k), Typeable k, Typeable a, Unjson a) => UnjsonDef (k -> a)
unjsonTotalMap = unjsonTotalMapBy unjsonDef

unjsonTotalMapBy :: (Typeable a, Eq k, Typeable k, (Constructors k, GEnum StandardEnum (Rep k), GBounded (Rep k))) => UnjsonDef a -> UnjsonDef (k -> a)
unjsonTotalMapBy def =
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

    allValues = genumFromTo gminBound gmaxBound

-- | This is encoded as a list of pairs
mapOfBy ::
  forall k v.
  (Ord k, Typeable k, Typeable v) =>
  UnjsonDef k ->
  UnjsonDef v ->
  UnjsonDef (Map k v)
mapOfBy uk uv =
  invmap M.fromList M.toList $
    arrayWithPrimaryKeyOf fst uk (unjsonTuple2By uk uv)

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
      let (projectConfig, localPackages, progSearchPath) = k :: (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])
      let (elaboratedInstallPlan, elaboratedSharedConfig, totalIndexState, activeRepos) = v :: (ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

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

withCacheFile ::
  (Binary a, Structured a, Binary b, Structured b) =>
  FilePath ->
  (Either String (MonitorStateFileSet, a, Either String b) -> IO r) ->
  IO r
withCacheFile cacheFile k =
  withBinaryFile cacheFile ReadMode $ \hnd -> do
    contents <- structuredDecodeTriple <$> BL.hGetContents hnd
    k contents

structuredDecodeTriple ::
  forall a b c.
  (Structured a, Structured b, Structured c, Binary a, Binary b, Binary c) =>
  BL.ByteString ->
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
