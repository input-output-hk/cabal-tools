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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Applicative.Free (Ap, hoistAp)
import Control.Monad (unless)
import Data.Aeson (GFromJSON, GToJSON', Value, Zero)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary (get), Get)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Functor.Contravariant ()
import Data.Functor.Identity ()
import Data.Functor.Invariant (invmap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.Unjson (
    FieldDef (..),
    Options (..),
    Result,
    Unjson (..),
    UnjsonDef (SimpleUnjsonDef),
    arrayOf,
    arrayWithPrimaryKeyOf,
    disjointUnionOf,
    enumOf,
    field,
    fieldBy,
    fieldOpt,
    objectOf,
    parse,
    render,
    unjsonAesonWithDoc,
    unjsonInvmapR,
    unjsonToByteStringLazy',
    unjsonToJSON,
    unjsonToJSON',
 )

import Distribution.Backpack (OpenModule, OpenUnitId)
import Distribution.Backpack.ConfiguredComponent ()
import Distribution.Backpack.ModuleShape (ModuleShape (..))
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Compat.Newtype ()
import Distribution.License qualified
import Distribution.ModuleName (ModuleName)
import Distribution.PackageDescription
import Distribution.Parsec (
    Parsec (..),
    eitherParsec,
    explicitEitherParsec,
 )
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.SPDX.License qualified
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PkgconfigVersion (PkgconfigVersion (..))
import Distribution.Types.SourceRepo (RepoType, SourceRepo)
import Distribution.Utils.Path
import Distribution.Utils.ShortText (ShortText, fromShortText, toShortText)
import Distribution.Utils.Structured (Structured, Tag)
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity

import Distribution.Simple (
    AbiHash,
    CompilerFlavor,
    ComponentId,
    DebugInfoLevel,
    Language,
    Module,
    OptimisationLevel,
    PackageDB,
    PackageId,
    PackageName,
    PerCompilerFlavor,
    PkgconfigName,
    ProfDetailLevel,
    UnitId,
    Version,
    VersionRange,
 )
import Distribution.Simple.Command (CommandParse (..), CommandUI (..), commandParseArgs)
import Distribution.Simple.Compiler (AbiTag, Compiler, CompilerFlag, CompilerId, PackageDB)
import Distribution.Simple.InstallDirs (
    InstallDirs,
    PathTemplate,
    fromPathTemplate,
    toPathTemplate,
 )
import Distribution.Simple.Setup (
    ConfigFlags (..),
    DumpBuildInfo,
    HaddockTarget,
    TestShowDetails,
    fromFlagOrDefault,
    readPackageDb,
    readPackageDbList,
    showPackageDb,
    showPackageDbList,
 )

import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.GlobalFlags
import Distribution.Client.HashValue (HashValue, showHashValue)
import Distribution.Client.IndexUtils (ActiveRepos, RepoIndexState, TotalIndexState)
import Distribution.Client.InstallPlan (GenericPlanPackage (..), toList)
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.PackageHash ()
import Distribution.Client.ProjectConfig (MapMappend (..), ProjectConfig, commandLineFlagsToProjectConfig)
import Distribution.Client.ProjectOrchestration (CurrentCommand (..), ProjectBaseContext (..), establishProjectBaseContext)
import Distribution.Client.ProjectPlanning (
    BuildStyle,
    ComponentTarget (..),
    ElaboratedConfiguredPackage (..),
    ElaboratedInstallPlan,
    ElaboratedPlanPackage,
    ElaboratedSharedConfig,
    SubComponentTarget (WholeComponent),
    rebuildInstallPlan,
 )
import Distribution.Client.ProjectPlanning.Types (
    ElaboratedComponent (..),
    ElaboratedPackage,
    ElaboratedPackageOrComponent (..),
    SetupScriptStyle,
 )
import Distribution.Client.Types (
    ConfiguredId (..),
    LocalRepo,
    PackageLocation (..),
    PackageSpecifier,
    RemoteRepo,
    Repo (RepoLocalNoIndex, RepoRemote, RepoSecure),
    UnresolvedSourcePackage,
 )
import Distribution.Client.Types.SourceRepo (
    SourceRepositoryPackage (..),
 )

import Distribution.Solver.Types.ComponentDeps qualified as CD
import Distribution.Solver.Types.OptionalStanza (
    OptionalStanza (..),
    OptionalStanzaMap,
    OptionalStanzaSet,
    optStanzaIndex,
    optStanzaSetFromList,
    optStanzaSetToList,
    optStanzaTabulate,
 )

import GHC.Generics (
    C1,
    Constructor (..),
    D1,
    Datatype (..),
    Generic (..),
    K1 (..),
    M1 (..),
    Meta (..),
    S1,
    Selector (selName),
    type (:*:) (..),
 )
import GHC.TypeLits ()
import Generic.Data (
    Constructors,
    GBounded,
    GDatatype,
    GEnum,
    GenericProduct (..),
    MetaConsRecord,
    MetaOf,
    MetaSelName,
    StandardEnum,
    gconName,
    gdatatypeName,
    genumFromTo,
    gmaxBound,
    gminBound,
 )
import Generic.Data.Internal.Meta (GDatatype (..))
import Language.Haskell.Extension
import Network.URI (URI, parseURI)
import System.Environment
import System.IO (IOMode (ReadMode), withBinaryFile)
import Text.Read (readMaybe)

import Data.Aeson.Text qualified as Aeson
import Data.Text.Lazy.IO qualified as TL
import Distribution.Client.Types.RepoName
import Distribution.System
import Distribution.Types.InstalledPackageInfo
import Text.Pretty.Simple (CheckColorTty (NoCheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsDarkBg, pPrintOpt)

-- Note: contramapFieldDef and contramapTupleFieldDef are basically
-- Contravariant, but due to type parameters in wrong order we would
-- need to do some type shuffling to get it right. Easier to just
-- write it here as it is.
contramapFieldDef :: (b -> a) -> FieldDef a x -> FieldDef b x
contramapFieldDef f (FieldReqDef name doc ext d) = FieldReqDef name doc (ext . f) d
contramapFieldDef f (FieldOptDef name doc ext d) = FieldOptDef name doc (ext . f) d
contramapFieldDef f (FieldDefDef name doc def ext d) = FieldDefDef name doc def (ext . f) d
contramapFieldDef f (FieldRODef name doc ext d) = FieldRODef name doc (ext . f) d

class FieldDef' a where
    fieldDef' :: Ap (FieldDef a) a
    default fieldDef' :: (Generic a, GFieldDef' (Rep a)) => Ap (FieldDef a) a
    fieldDef' = to <$> hoistAp (contramapFieldDef from) gFieldDef'

gfieldDef' :: (Generic a, GFieldDef' (Rep a)) => Ap (FieldDef a) a
gfieldDef' = to <$> hoistAp (contramapFieldDef from) gFieldDef'

class GFieldDef' f where
    gFieldDef' :: Ap (FieldDef (f p)) (f p)

instance GFieldDef' f => GFieldDef' (D1 t f) where
    gFieldDef' = M1 <$> hoistAp (contramapFieldDef unM1) gFieldDef'

instance GFieldDef' f => GFieldDef' (C1 t f) where
    gFieldDef' = M1 <$> hoistAp (contramapFieldDef unM1) gFieldDef'

data HProxy s (f :: Type -> Type) a = HProxy

-- FIXME: selector names can be null
instance {-# OVERLAPPING #-} (Typeable c, Unjson c, Selector t) => GFieldDef' (S1 t (K1 i (Maybe c))) where
    gFieldDef' = M1 . K1 <$> hoistAp (contramapFieldDef (unK1 . unM1)) (fieldOpt name id name)
      where
        name = T.pack $ selName (HProxy :: HProxy t f a)

instance (Typeable c, Unjson c, Selector t) => GFieldDef' (S1 t (K1 i c)) where
    gFieldDef' = M1 . K1 <$> hoistAp (contramapFieldDef (unK1 . unM1)) (field name id name)
      where
        name = T.pack $ selName (HProxy :: HProxy t f a)

instance (GFieldDef' l, GFieldDef' r) => GFieldDef' (l :*: r) where
    gFieldDef' :: Ap (FieldDef ((l :*: r) p)) ((l :*: r) p)
    gFieldDef' = liftA2 (:*:) l r
      where
        l = hoistAp (contramapFieldDef fst') gFieldDef'
        r = hoistAp (contramapFieldDef snd') gFieldDef'
        fst' (f :*: _) = f
        snd' (_ :*: g) = g

newtype UnjsonObject a = UnjsonObject a

instance (Generic a, GFieldDef' (Rep a)) => Unjson (UnjsonObject a) where
    unjsonDef = invmap coerce coerce $ objectOf $ gfieldDef' @a

--
-- Enumeration
--

newtype UnjsonEnumeration a = UnjsonEnumeration a
    deriving (Eq, Generic)

instance
    ( Eq a
    , Constructors a
    , GEnum StandardEnum (Rep a)
    , GBounded (Rep a)
    , GDatatype (Rep a)
    )
    => Unjson (UnjsonEnumeration a)
    where
    unjsonDef =
        invmap coerce coerce $
            enumOf @a
                (T.pack $ gdatatypeName @a)
                [(T.pack $ gconName bs, bs) | bs <- genumFromTo gminBound gmaxBound]

--
-- PrettyParsec
--

newtype UnjsonPrettyParsec a = UnjsonPrettyParsec a

deriving via (a :: Type) instance Pretty a => Pretty (UnjsonPrettyParsec a)

-- | NOTE: This does not work
-- deriving via a instance (Parsec a) => Parsec (UnjsonPrettyParsec a)
instance Parsec a => Parsec (UnjsonPrettyParsec a) where
    parsec = UnjsonPrettyParsec <$> parsec

instance (Pretty a, Parsec a) => Unjson (UnjsonPrettyParsec a) where
    unjsonDef = unjsonInvmapR (either fail return . eitherParsec) prettyShow unjsonDef

--
-- ShowRead
--

newtype UnjsonShowRead a = UnjsonShowRead a

deriving via (a :: Type) instance Show a => Show (UnjsonShowRead a)

deriving via (a :: Type) instance Read a => Read (UnjsonShowRead a)

instance (Show a, Read a) => Unjson (UnjsonShowRead a) where
    unjsonDef = unjsonInvmapR (maybe (fail "cannot parse") return . readMaybe) show unjsonDef

--
-- Aeson
--

newtype UnjsonGenericAeson a = UnjsonGenericAeson a

instance (Generic a, GFromJSON Zero (Rep a), GToJSON' Value Zero (Rep a)) => Unjson (UnjsonGenericAeson a) where
    unjsonDef = invmap coerce coerce $ unjsonGenericAeson @a "" Aeson.defaultOptions

--
-- Ad hoc types
--

unjsonTotalMap
    :: ( Eq k
       , Constructors k
       , GEnum StandardEnum (Rep k)
       , GBounded (Rep k)
       , Typeable k
       , Typeable a
       , Unjson a
       )
    => UnjsonDef (k -> a)
unjsonTotalMap = unjsonTotalMapBy unjsonDef

unjsonTotalMapBy
    :: ( Typeable a
       , Eq k
       , Typeable k
       , (Constructors k, GEnum StandardEnum (Rep k), GBounded (Rep k))
       )
    => UnjsonDef a
    -> UnjsonDef (k -> a)
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
mapOfBy
    :: forall k v
     . (Ord k, Typeable k, Unjson k, Typeable v, Unjson v)
    => UnjsonDef (Map k v)
mapOfBy =
    invmap M.fromList M.toList $
        arrayWithPrimaryKeyOf fst unjsonDef unjsonDef -- (unjsonTuple2By uk uv)

--
-- Instances
--

deriving via UnjsonEnumeration BuildStyle instance Unjson BuildStyle
deriving via UnjsonEnumeration DebugInfoLevel instance Unjson DebugInfoLevel
deriving via UnjsonEnumeration DumpBuildInfo instance Unjson DumpBuildInfo
deriving via UnjsonEnumeration LibraryVisibility instance Unjson LibraryVisibility
deriving via UnjsonEnumeration OptimisationLevel instance Unjson OptimisationLevel
deriving via UnjsonEnumeration OptionalStanza instance Unjson OptionalStanza
deriving via UnjsonEnumeration SetupScriptStyle instance Unjson SetupScriptStyle
deriving via UnjsonObject (InstallDirs a) instance (Unjson a, Typeable a) => Unjson (InstallDirs a)
deriving via UnjsonObject (SourceRepositoryPackage Maybe) instance Unjson (SourceRepositoryPackage Maybe)
deriving via UnjsonObject (SourceRepositoryPackage []) instance Unjson (SourceRepositoryPackage [])
deriving via UnjsonObject Benchmark instance Unjson Benchmark
deriving via UnjsonObject BuildInfo instance Unjson BuildInfo
deriving via UnjsonObject Compiler instance Unjson Compiler
deriving via UnjsonObject CompilerId instance Unjson CompilerId
deriving via UnjsonObject ConfiguredId instance Unjson ConfiguredId
deriving via UnjsonObject ElaboratedComponent instance Unjson ElaboratedComponent
deriving via UnjsonObject ElaboratedConfiguredPackage instance Unjson ElaboratedConfiguredPackage
deriving via UnjsonObject ElaboratedPackage instance Unjson ElaboratedPackage

-- deriving via UnjsonObject ElaboratedSharedConfig instance Unjson ElaboratedSharedConfig
deriving via UnjsonObject Executable instance Unjson Executable
deriving via UnjsonObject ForeignLib instance Unjson ForeignLib
deriving via UnjsonObject InstalledPackageInfo instance Unjson InstalledPackageInfo
deriving via UnjsonObject Library instance Unjson Library
deriving via UnjsonObject ModuleReexport instance Unjson ModuleReexport
deriving via UnjsonObject ModuleShape instance Unjson ModuleShape
deriving via UnjsonObject PackageDescription instance Unjson PackageDescription
deriving via UnjsonObject SetupBuildInfo instance Unjson SetupBuildInfo
deriving via UnjsonObject SourceRepo instance Unjson SourceRepo
deriving via UnjsonObject TestSuite instance Unjson TestSuite
deriving via UnjsonObject TotalIndexState instance Unjson TotalIndexState
deriving via UnjsonPrettyParsec ActiveRepos instance Unjson ActiveRepos

-- deriving via UnjsonObject ProjectConfig instance Unjson ProjectConfig

deriving via UnjsonPrettyParsec (SymbolicPath a b) instance Unjson (SymbolicPath a b)
deriving via UnjsonPrettyParsec AbiDependency instance Unjson AbiDependency
deriving via UnjsonPrettyParsec AbiHash instance Unjson AbiHash
deriving via UnjsonPrettyParsec AbiTag instance Unjson AbiTag
deriving via UnjsonPrettyParsec BuildType instance Unjson BuildType
deriving via UnjsonPrettyParsec CompilerFlavor instance Unjson CompilerFlavor
deriving via UnjsonPrettyParsec ComponentId instance Unjson ComponentId
deriving via UnjsonPrettyParsec ComponentName instance Unjson ComponentName
deriving via UnjsonPrettyParsec Dependency instance Unjson Dependency
deriving via UnjsonPrettyParsec Distribution.SPDX.License.License instance Unjson Distribution.SPDX.License.License
deriving via UnjsonPrettyParsec ExeDependency instance Unjson ExeDependency
deriving via UnjsonPrettyParsec ExecutableScope instance Unjson ExecutableScope
deriving via UnjsonPrettyParsec ExposedModule instance Unjson ExposedModule
deriving via UnjsonPrettyParsec Extension instance Unjson Extension
deriving via UnjsonPrettyParsec FlagAssignment instance Unjson FlagAssignment
deriving via UnjsonPrettyParsec ForeignLibOption instance Unjson ForeignLibOption
deriving via UnjsonPrettyParsec ForeignLibType instance Unjson ForeignLibType
deriving via UnjsonPrettyParsec HaddockTarget instance Unjson HaddockTarget
deriving via UnjsonPrettyParsec Language instance Unjson Language
deriving via UnjsonPrettyParsec LegacyExeDependency instance Unjson LegacyExeDependency
deriving via UnjsonPrettyParsec LibVersionInfo instance Unjson LibVersionInfo
deriving via UnjsonPrettyParsec LocalRepo instance Unjson LocalRepo
deriving via UnjsonPrettyParsec Mixin instance Unjson Mixin
deriving via UnjsonPrettyParsec Module instance Unjson Module
deriving via UnjsonPrettyParsec ModuleName instance Unjson ModuleName
deriving via UnjsonPrettyParsec OpenModule instance Unjson OpenModule
deriving via UnjsonPrettyParsec OpenUnitId instance Unjson OpenUnitId
deriving via UnjsonPrettyParsec PackageId instance Unjson PackageId
deriving via UnjsonPrettyParsec PackageName instance Unjson PackageName
deriving via UnjsonPrettyParsec Platform instance Unjson Platform
deriving via UnjsonPrettyParsec PkgconfigDependency instance Unjson PkgconfigDependency
deriving via UnjsonPrettyParsec PkgconfigName instance Unjson PkgconfigName
deriving via UnjsonPrettyParsec PkgconfigVersion instance Unjson PkgconfigVersion
deriving via UnjsonPrettyParsec RemoteRepo instance Unjson RemoteRepo
deriving via UnjsonPrettyParsec RepoName instance Unjson RepoName
deriving via UnjsonPrettyParsec RepoIndexState instance Unjson RepoIndexState
deriving via UnjsonPrettyParsec RepoKind instance Unjson RepoKind
deriving via UnjsonPrettyParsec RepoType instance Unjson RepoType
deriving via UnjsonPrettyParsec TestShowDetails instance Unjson TestShowDetails
deriving via UnjsonPrettyParsec TestType instance Unjson TestType
deriving via UnjsonPrettyParsec UnitId instance Unjson UnitId
deriving via UnjsonPrettyParsec UnqualComponentName instance Unjson UnqualComponentName
deriving via UnjsonPrettyParsec Version instance Unjson Version
deriving via UnjsonPrettyParsec VersionRange instance Unjson VersionRange
deriving via UnjsonShowRead (Maybe Bool) instance Unjson (Maybe Bool)
deriving via UnjsonShowRead (Maybe CompilerFlag) instance Unjson (Maybe CompilerFlag)
deriving via UnjsonShowRead BenchmarkInterface instance Unjson BenchmarkInterface
deriving via UnjsonShowRead CabalSpecVersion instance Unjson CabalSpecVersion
deriving via UnjsonShowRead PackageDB instance Unjson PackageDB

deriving via Map a b instance Unjson (Map a b) => Unjson (MapMappend a b)

-- | FIXME
deriving via UnjsonShowRead LibraryName instance Unjson LibraryName

-- | FIXME
deriving via UnjsonShowRead (Either Distribution.SPDX.License.License Distribution.License.License) instance Unjson (Either Distribution.SPDX.License.License Distribution.License.License)

instance {-# INCOHERENT #-} Unjson (PkgconfigName, Maybe PkgconfigVersion) where
    unjsonDef = objectOf gfieldDef'

instance (Typeable a, Unjson a) => Unjson (Map ModuleName a) where
    unjsonDef = mapOfBy

instance (Typeable a, Unjson a) => Unjson (Map RepoName a) where
    unjsonDef = mapOfBy

instance (Typeable a, Unjson a) => Unjson (Map PackageName a) where
    unjsonDef = mapOfBy

instance Unjson (Maybe PackageDB) where
    unjsonDef = invmap readPackageDb showPackageDb unjsonDef

deriving via UnjsonShowRead ComponentRequestedSpec instance Unjson ComponentRequestedSpec

gConst :: (Generic a, Constructors b, Unjson a, Typeable a, Generic b) => String -> Text -> (a -> b) -> (s -> a) -> (Text, b -> Bool, Ap (FieldDef s) b)
gConst name doc cons proj = (T.pack name, (== name) . gconName, cons <$> field (T.pack name) proj doc)

-- instance Unjson ComponentRequestedSpec where
--     unjsonDef =
--         disjointUnionOf
--             "ComponentRequestedSpec"
--             [ gConst "ComponentRequestedSpec" "ComponentRequestedSpec" ComponentRequestedSpec (\case ~(ComponentRequestedSpec test bench) -> _)
--             , gConst "OneComponentRequestedSpec" "OneComponentRequestedSpec" OneComponentRequestedSpec (\case ~(OneComponentRequestedSpec name) -> name)
--             ]

instance Unjson ElaboratedPlanPackage where
    unjsonDef =
        disjointUnionOf
            "ElaboratedPlanPackage"
            [ gConst "PreExisting" "PreExisting" PreExisting (\case ~(PreExisting pkg) -> pkg)
            , gConst "Configured" "Configured" Configured (\case ~(Configured pkg) -> pkg)
            , gConst "Installed" "Installed" Installed (\case ~(Installed pkg) -> pkg)
            ]

instance Unjson ElaboratedPackageOrComponent where
    unjsonDef =
        disjointUnionOf
            "ElaboratedPackageOrComponent"
            [ gConst "ElabPackage" "ElabPackage" ElabPackage (\case ~(ElabPackage pkg) -> pkg)
            , gConst "ElabComponent" "ElabComponent" ElabComponent (\case ~(ElabComponent pkg) -> pkg)
            ]

instance (Unjson a, Typeable a) => Unjson (PackageLocation (Maybe a)) where
    unjsonDef =
        disjointUnionOf
            "PackageLocation"
            [
                ( "LocalUnpackedPackage"
                , (== "LocalUnpackedPackage") . gconName
                , LocalUnpackedPackage
                    <$> field
                        "path"
                        (\case ~(LocalUnpackedPackage fp) -> fp)
                        "path"
                )
            ,
                ( "LocalTarballPackage"
                , (== "LocalTarballPackage") . gconName
                , LocalTarballPackage
                    <$> field
                        "path"
                        (\case ~(LocalTarballPackage fp) -> fp)
                        "path"
                )
            ,
                ( "RemoteTarballPackage"
                , (== "RemoteTarballPackage") . gconName
                , RemoteTarballPackage
                    <$> field
                        "uri"
                        (\case ~(RemoteTarballPackage uri _fp) -> uri)
                        "uri"
                    <*> fieldOpt
                        "path"
                        (\case ~(RemoteTarballPackage _uri fp) -> fp)
                        "path"
                )
            ,
                ( "RepoTarballPackage"
                , (== "RepoTarballPackage") . gconName
                , RepoTarballPackage
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
                )
            ,
                ( "RemoteSourceRepoPackage"
                , (== "RemoteSourceRepoPackage") . gconName
                , RemoteSourceRepoPackage
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
        [
            ( "RepoLocalNoIndex"
            , (== "RepoLocalNoIndex") . gconName
            , RepoLocalNoIndex
                <$> field
                    "repoLocal"
                    (\case ~(RepoLocalNoIndex repo _fp) -> repo)
                    "repoLocal"
                <*> field
                    "repoLocalDir"
                    (\case ~(RepoLocalNoIndex _repo fp) -> fp)
                    "repoLocalDir"
            )
        ,
            ( "RepoRemote"
            , (== "RepoRemote") . gconName
            , RepoRemote
                <$> field
                    "repoRemote"
                    (\case ~(RepoRemote repo _fp) -> repo)
                    "repoRemote"
                <*> field
                    "repoLocalDir"
                    (\case ~(RepoRemote _repo fp) -> fp)
                    "repoLocalDir"
            )
        ,
            ( "RepoSecure"
            , (== "RepoSecure") . gconName
            , RepoSecure
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

instance Unjson OptionalStanzaSet where
    unjsonDef = invmap optStanzaSetFromList optStanzaSetToList unjsonDef

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

instance (Monoid a, Typeable a, Unjson a) => Unjson (CD.ComponentDeps a) where
    unjsonDef = invmap CD.fromList CD.toList unjsonDef

-- | TODO: Fixing SubComponentTarget = WholeComponent for the moment
instance Unjson ComponentTarget where
    unjsonDef =
        invmap
            (`ComponentTarget` WholeComponent)
            (\(ComponentTarget cn _) -> cn)
            unjsonDef

instance Unjson TestSuiteInterface where
    unjsonDef =
        disjointUnionOf
            "TestSuiteExeV10"
            [
                ( "TestSuiteExeV10"
                , (== "TestSuiteExeV10") . gconName
                , TestSuiteExeV10
                    <$> field "Version" (\(TestSuiteExeV10 v _) -> v) "Version"
                    <*> field "FilePath" (\(TestSuiteExeV10 _ p) -> p) "FilePath"
                )
            ,
                ( "TestSuiteLibV09"
                , (== "TestSuiteLibV09") . gconName
                , TestSuiteLibV09
                    <$> field "Version" (\(TestSuiteLibV09 v _) -> v) "Version"
                    <*> field "ModulePath" (\(TestSuiteLibV09 _ m) -> m) "ModulePath"
                )
            ,
                ( "TestSuiteUnsupported"
                , (== "TestSuiteUnsupported") . gconName
                , TestSuiteUnsupported
                    <$> field "TestType" (\(TestSuiteUnsupported t) -> t) "TestType"
                )
            ]

instance (Typeable v, Unjson v) => Unjson (PerCompilerFlavor v) where
    unjsonDef = objectOf gfieldDef'

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

instance Unjson ShortText where
    unjsonDef = invmap toShortText fromShortText unjsonDef

type GFromToJSON a = (GFromJSON Zero a, GToJSON' Value Zero a)

unjsonGenericAeson :: (Generic a, GFromToJSON (Rep a)) => Text -> Aeson.Options -> UnjsonDef a
unjsonGenericAeson docstring options =
    SimpleUnjsonDef
        docstring
        (either fail return . Aeson.parseEither (Aeson.genericParseJSON options))
        (Aeson.genericToJSON options)

-- -- | Workaround because TupleFieldDef is not exposed
-- unjsonTuple2By :: UnjsonDef k -> UnjsonDef v -> UnjsonDef (k, v)
-- unjsonTuple2By uk uv =
--   unjsonInvmapR
--     (\(vk, vv) -> (,) <$> parse uk vk <*> parse uv vv)
--     (bimap (unjsonToJSON uk) (unjsonToJSON uv))
--     unjsonDef

main :: IO ()
main = do
    args <- getArgs
    case commandParseArgs cmdUI True args of
        CommandHelp help -> putStrLn (help "cabal-make-install-plan")
        CommandList opts -> putStrLn $ "commandList" ++ show opts
        CommandErrors errs -> putStrLn $ "commandErrors: " ++ show errs
        CommandReadyToGo (mkflags, _commandParse) ->
            let globalFlags = defaultGlobalFlags
                flags@NixStyleFlags{configFlags} = mkflags (commandDefaultFlags cmdUI)
                verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
                cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
             in makeInstallPlanAction verbosity cliConfig

cmdUI :: CommandUI (NixStyleFlags ())
cmdUI =
    CommandUI
        { commandName = "make-install-plan"
        , commandSynopsis = "It makes an install-plan"
        , commandUsage = ("Usage: " ++)
        , commandDescription = Nothing
        , commandNotes = Nothing
        , commandDefaultFlags = defaultNixStyleFlags ()
        , commandOptions = nixStyleOptions (const [])
        }

-- The following is adapted from cabal-install's Distribution.Client.CmdFreeze
makeInstallPlanAction :: Verbosity -> ProjectConfig -> IO ()
makeInstallPlanAction verbosity cliConfig = do
    ProjectBaseContext{distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
        establishProjectBaseContext verbosity cliConfig OtherCommand

    (_improvedPlan, elaboratedPlan, _elaboratedSharedConfig, totalIndexState, activeRepos) <-
        rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing

    -- putStrLn "-------------------- projectConfig --------------------"
    -- printJson projectConfig

    -- putStrLn "-------------------- localPackages --------------------"
    -- printJson localPackages
    --
    TL.putStrLn $
        Aeson.encodeToLazyText $
            Aeson.object
                [ "elaboratedPlan" Aeson..= map (unjsonToJSON unjsonDef) (toList elaboratedPlan)
                , "totalIndexState" Aeson..= unjsonToJSON unjsonDef totalIndexState
                , "activeRepos" Aeson..= unjsonToJSON unjsonDef activeRepos
                ]

printJson :: Unjson a => a -> IO ()
printJson = BL.putStr . unjsonToByteStringLazy' (Options{pretty = True, indent = 2, nulls = True}) unjsonDef
