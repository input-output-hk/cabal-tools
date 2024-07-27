{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Unjson (module Unjson, module Data.Unjson)
where

import Data.Coerce
import Data.Functor.Invariant
import Data.Kind
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)
import Data.Maybe
import Data.Traversable
import Data.Typeable
import GHC.Generics
import Text.Read
import Type.Reflection hiding (typeRep)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as TL

import Distribution.Parsec
import Distribution.Pretty

import Control.Applicative.Free
import Data.Aeson.Types qualified as Aeson
import Data.Unjson
import Generic.Data

--
-- FieldDef
--

-- Note: contramapFieldDef is basically contramap, but due to type parameters
-- in wrong order we would need to do some type shuffling to get it right.
-- Easier to just write it here as it is.
contramapFieldDef :: (t -> s) -> FieldDef s a -> FieldDef t a
contramapFieldDef f (FieldReqDef name doc ext d) = FieldReqDef name doc (ext . f) d
contramapFieldDef f (FieldOptDef name doc ext d) = FieldOptDef name doc (ext . f) d
contramapFieldDef f (FieldDefDef name doc def ext d) = FieldDefDef name doc def (ext . f) d
contramapFieldDef f (FieldRODef name doc ext d) = FieldRODef name doc (ext . f) d

dimapApFieldDef :: (t -> s) -> (a -> b) -> Ap (FieldDef s) a -> Ap (FieldDef t) b
dimapApFieldDef f g = fmap g . hoistAp (contramapFieldDef f)

genericFieldDef :: (Generic a, GFieldDef (Rep a)) => Ap (FieldDef a) a
genericFieldDef = dimapApFieldDef from to gFieldDef

class GFieldDef f where
    gFieldDef :: Ap (FieldDef (f p)) (f p)

instance GFieldDef'C f => GFieldDef (D1 (d :: Meta) f) where
    gFieldDef = dimapApFieldDef unM1 M1 gFieldDef'C

class GFieldDef'C f where
    gFieldDef'C :: Ap (FieldDef (f p)) (f p)

instance (GFieldDef'S f, Constructor m) => GFieldDef'C (C1 m f) where
    gFieldDef'C = dimapApFieldDef unM1 M1 $ gFieldDef'S cn
      where
        cn = T.pack $ conName (undefined :: t m f a)

class GFieldDef'S f where
    gFieldDef'S :: T.Text -> Ap (FieldDef (f p)) (f p)

instance (GFieldDef'K f, Selector m) => GFieldDef'S (S1 m f) where
    gFieldDef'S cn = dimapApFieldDef unM1 M1 $ gFieldDef'K cn sn
      where
        sn = T.pack $ selName (undefined :: t m f a)

instance (GFieldDef'S l, GFieldDef'S r) => GFieldDef'S (l :*: r) where
    gFieldDef'S cn = liftA2 (:*:) l r
      where
        l = hoistAp (contramapFieldDef fst') (gFieldDef'S cn)
        r = hoistAp (contramapFieldDef snd') (gFieldDef'S cn)
        fst' (f :*: _) = f
        snd' (_ :*: g) = g

class GFieldDef'K f where
    gFieldDef'K :: T.Text -> T.Text -> Ap (FieldDef (f p)) (f p)

instance {-# OVERLAPPABLE #-} (Unjson a, Typeable a) => GFieldDef'K (K1 i a) where
    gFieldDef'K _cn sn = dimapApFieldDef unK1 K1 $ field sn' id sn
      where
        sn'
            | sn == "" = T.pack $ tyConName $ someTypeRepTyCon $ typeRep (Proxy :: Proxy a)
            | otherwise = sn

instance {-# OVERLAPPING #-} (Unjson a, Typeable a) => GFieldDef'K (K1 i (Maybe a)) where
    gFieldDef'K _cn sn = dimapApFieldDef unK1 K1 $ fieldOpt sn id sn

newtype UnjsonObject a = UnjsonObject a

instance (Generic a, GFieldDef (Rep a)) => Unjson (UnjsonObject a) where
    unjsonDef = invmap coerce coerce $ objectOf $ genericFieldDef @a

--
-- Enumeration
--

newtype UnjsonEnum a = UnjsonEnum a
    deriving (Eq, Generic)

instance (Eq a, Typeable a, Enum a, Bounded a, Constructors a, GDatatype (Rep a)) => Unjson (UnjsonEnum a) where
    unjsonDef =
        invmap coerce coerce $
            enumOf @a
                (T.pack $ gdatatypeName @a)
                [(T.pack $ gconName bs, bs) | bs <- enumFromTo minBound maxBound]

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

instance (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a), Aeson.GToJSON' Aeson.Value Aeson.Zero (Rep a)) => Unjson (UnjsonGenericAeson a) where
    unjsonDef = invmap coerce coerce $ unjsonGenericAeson @a "" Aeson.defaultOptions

type GFromToJSON a = (Aeson.GFromJSON Aeson.Zero a, Aeson.GToJSON' Aeson.Value Aeson.Zero a)

unjsonGenericAeson :: (Generic a, GFromToJSON (Rep a)) => T.Text -> Aeson.Options -> UnjsonDef a
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
    => UnjsonDef (M.Map k v)
mapOfBy =
    invmap M.fromList M.toList $
        arrayWithPrimaryKeyOf fst unjsonDef unjsonDef -- (unjsonTuple2By uk uv)

gConst :: (Generic a, Constructors b, Unjson a, Typeable a, Generic b) => String -> T.Text -> (a -> b) -> (s -> a) -> (T.Text, b -> Bool, Ap (FieldDef s) b)
gConst name doc cons proj = (T.pack name, (== name) . gconName, cons <$> field (T.pack name) proj doc)

isConstrByName :: (Generic a, GConstructors (Rep a)) => String -> a -> Bool
isConstrByName name = (== name) . gconName

deriving via UnjsonShowRead (Maybe Bool) instance Unjson (Maybe Bool)

instance Unjson BS.ByteString where
    unjsonDef = invmap T.encodeUtf8 T.decodeUtf8 unjsonDef

instance Unjson BL.ByteString where
    unjsonDef = invmap TL.encodeUtf8 TL.decodeUtf8 unjsonDef

instance (Unjson a, Typeable a) => Unjson (NonEmpty a) where
    unjsonDef = unjsonInvmapR (maybeToProblem . nonEmpty) toList $ arrayWithModeOf @a ArrayModeStrict unjsonDef
      where
        maybeToProblem = maybe (fail "The array must non be empty.") pure
