{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Unjson.Generic where

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
    UnjsonDef (..),
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
 )
import Distribution.Backpack (OpenModule, OpenUnitId)
import Distribution.Backpack.ConfiguredComponent ()
import Distribution.Backpack.ModuleShape (ModuleShape (..))
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.HashValue (HashValue, showHashValue)
import Distribution.Client.IndexUtils (
    ActiveRepos,
    TotalIndexState,
 )
import Distribution.Client.InstallPlan (toList)
import Distribution.Client.PackageHash ()
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectPlanning (
    BuildStyle,
    ComponentTarget (..),
    ElaboratedConfiguredPackage (..),
    ElaboratedInstallPlan,
    ElaboratedSharedConfig,
    SubComponentTarget (WholeComponent),
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
import Distribution.Simple (
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
import Distribution.Simple.Compiler (PackageDB)
import Distribution.Simple.Flag (Flag, flagToMaybe, maybeToFlag)
import Distribution.Simple.InstallDirs (
    InstallDirs,
    PathTemplate,
    fromPathTemplate,
    toPathTemplate,
 )
import Distribution.Simple.Setup (
    DumpBuildInfo,
    HaddockTarget,
    TestShowDetails,
    readPackageDb,
    readPackageDbList,
    showPackageDb,
    showPackageDbList,
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
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (..))
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.PackageDescription (PackageDescription)
import Distribution.Types.PkgconfigVersion (PkgconfigVersion (..))
import Distribution.Types.SourceRepo (RepoType, SourceRepo)
import Distribution.Utils.Path
import Distribution.Utils.ShortText (ShortText, fromShortText, toShortText)
import Distribution.Utils.Structured (Structured, Tag)
import GHC.Generics
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
import Language.Haskell.Extension
import Network.URI (URI, parseURI)
import System.IO (IOMode (ReadMode), withBinaryFile)
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

-- data    V1        p                       -- lifted version of Empty
-- data    U1        p = U1                  -- lifted version of ()
-- data    (:+:) f g p = L1 (f p) | R1 (g p) -- lifted version of Either
-- data    (:*:) f g p = (f p) :*: (g p)     -- lifted version of (,)
-- newtype K1    i c p = K1 { unK1 :: c }    -- a container for a c
-- newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper

-- class FieldDef' a where
--     fieldDef' :: Ap (FieldDef a) a
--     default fieldDef' :: (Generic a, GFieldDef' (Rep a)) => Ap (FieldDef a) a
--     fieldDef' = to <$> hoistAp (contramapFieldDef from) gFieldDef

hoistField :: (a -> b) -> (c -> d) -> Ap (FieldDef d) a -> Ap (FieldDef c) b
hoistField to from = fmap to . hoistAp (contramapFieldDef from)

myFieldDef :: (Generic a, GFieldDef (Rep a)) => Ap (FieldDef a) a
myFieldDef = hoistField to from gFieldDef

class GFieldDef f where
    gFieldDef :: Ap (FieldDef (f p)) (f p)

instance GFieldDef f => GFieldDef (D1 t f) where
    gFieldDef = hoistField M1 unM1 gFieldDef

instance GFieldDef f => GFieldDef (C1 t f) where
    gFieldDef = hoistField M1 unM1 gFieldDef

data HProxy s (f :: Type -> Type) a = HProxy

-- | Maybe is a special case
instance {-# OVERLAPPING #-} (Typeable c, Unjson c, Selector t) => GFieldDef (S1 t (K1 i (Maybe c))) where
    gFieldDef = hoistField (M1 . K1) (unK1 . unM1) (fieldOpt name id name)
      where
        name = T.pack $ selName (HProxy :: HProxy t f a)

instance (Typeable c, Unjson c, Selector t) => GFieldDef (S1 t (K1 i c)) where
    gFieldDef = hoistField (M1 . K1) (unK1 . unM1) (field name id name)
      where
        name = T.pack $ selName (HProxy :: HProxy t f a)

instance (GFieldDef l, GFieldDef r) => GFieldDef (l :*: r) where
    gFieldDef :: Ap (FieldDef ((l :*: r) p)) ((l :*: r) p)
    gFieldDef = liftA2 (:*:) l r
      where
        l :: Ap (FieldDef ((l :*: r) p)) (l p)
        l = hoistAp (contramapFieldDef fst') gFieldDef
        r :: Ap (FieldDef ((l :*: r) p)) (r p)
        r = hoistAp (contramapFieldDef snd') gFieldDef
        fst' (f :*: _) = f
        snd' (_ :*: g) = g

data Ex = ExA {a :: Int} | ExB {b :: Bool}

myEnumOf :: (Generic a, GEnumOf (Rep a)) => UnjsonDef a
myEnumOf = invmap to from gEnumOf

-- g :: UnjsonDef Ex
-- g = gEnumOf

class GEnumOf f where
    gEnumOf :: UnjsonDef (f p)

instance GEnumOf f => GEnumOf (D1 t f) where
    gEnumOf = invmap M1 unM1 gEnumOf

-- instance GEnumOf f => GEnumOf (C1 t f) where
--     gEnumOf = invmap M1 unM1 gEnumOf

instance (GFieldDef l, GFieldDef r) => GEnumOf (l :+: r) where
    gEnumOf =
        DisjointUnjsonDef
            name
            [ (lText, lSel, lUnjson)
            , (rText, rSel, rUnjson)
            ]
      where
        name = T.pack "name"
        lText = T.pack ""
        lSel (L1 _) = True
        lSel (R1 _) = False
        lUnjson' = gFieldDef :: Ap (FieldDef (l p)) (l p)
        rSel (L1 _) = False
        rSel (R1 _) = True
        rText = T.pack ""
        rUnjson' = gFieldDef :: Ap (FieldDef (r p)) (r p)

class GConstr f where
    gConstr :: (Text, f p -> Bool, Ap (FieldDef (f p)) (Result (f p)))
