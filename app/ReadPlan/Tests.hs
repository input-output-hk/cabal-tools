{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ReadPlan.Tests where

import Control.Applicative.Free
import Data.Aeson as Aeson (GFromJSON, GToJSON', Zero)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Functor.Invariant (invmap)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Unjson
import Distribution.Client.Types.SourceRepo (SourceRepoList, sourceRepositoryPackageGrammar)
import Distribution.Compat.Lens (ALens', aview)
import Distribution.Compat.Newtype
import Distribution.Compiler (CompilerFlavor)
import Distribution.FieldGrammar
import Distribution.Fields.Field (FieldName)
import Distribution.PackageDescription (PackageDescription)
import Distribution.PackageDescription.FieldGrammar (packageDescriptionFieldGrammar, sourceRepoFieldGrammar)
import Distribution.Parsec (Parsec, eitherParsec)
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.Types.SourceRepo (SourceRepo)
import Distribution.Types.VersionRange (VersionRange)
import Distribution.Utils.Path (SymbolicPath)
import Distribution.Utils.ShortText (fromShortText, toShortText)
import GHC.Generics (Generic, Rep)
import Type.Reflection (Typeable)

class Typeable a => MyContraint a where
    myUnjsonDef :: UnjsonDef a

instance MyContraint FilePathNT where myUnjsonDef = invmap pack unpack unjsonDef
instance MyContraint SpecLicense where myUnjsonDef = unjsonPrettyParsec
instance MyContraint SpecVersion where myUnjsonDef = unjsonPrettyParsec
instance MyContraint TestedWith where myUnjsonDef = unjsonPrettyParsec
instance MyContraint Token where myUnjsonDef = unjsonPrettyParsec
instance MyContraint Token' where myUnjsonDef = unjsonPrettyParsec

newtype UnjsonFG s a = UnjsonFG (Ap (FieldDef s) a)

deriving via Ap (FieldDef s) instance Functor (UnjsonFG s)
deriving via Ap (FieldDef s) instance Applicative (UnjsonFG s)

fieldNameToText :: FieldName -> Text
fieldNameToText = T.decodeUtf8

instance FieldGrammar MyContraint UnjsonFG where
    blurFieldGrammar l (UnjsonFG f) =
        UnjsonFG $
            hoistAp (contramapFieldDef (aview l)) f

    uniqueFieldAla
        :: (MyContraint b, Newtype a b)
        => FieldName
        -> (a -> b)
        -> ALens' s a
        -> UnjsonFG s a
    uniqueFieldAla fn _pack l =
        UnjsonFG $
            fmap unpack $
                fieldBy
                    (fieldNameToText fn)
                    (_pack . aview l)
                    (fieldNameToText fn)
                    myUnjsonDef

    booleanFieldDef
        :: FieldName
        -> ALens' s Bool
        -> Bool
        -> UnjsonFG s Bool
    booleanFieldDef fn l def =
        UnjsonFG $
            fieldDefBy
                (fieldNameToText fn)
                def
                (aview l)
                (fieldNameToText fn)
                unjsonDef

    optionalFieldAla fn _pack l =
        UnjsonFG $
            fmap (fmap unpack) $
                fieldOptBy
                    (fieldNameToText fn)
                    (fmap _pack . aview l)
                    (fieldNameToText fn)
                    myUnjsonDef

    optionalFieldDefAla fn _pack l def =
        UnjsonFG $
            fmap unpack $
                fieldDefBy
                    (fieldNameToText fn)
                    (_pack def)
                    (_pack . aview l)
                    (fieldNameToText fn)
                    myUnjsonDef

    freeTextField fn l =
        UnjsonFG $
            fieldOpt
                (fieldNameToText fn)
                (aview l)
                (fieldNameToText fn)

    freeTextFieldDef fn l =
        UnjsonFG $
            fieldDef
                (fieldNameToText fn)
                ""
                (aview l)
                (fieldNameToText fn)

    freeTextFieldDefST fn l =
        UnjsonFG $
            fieldDefBy
                (fieldNameToText fn)
                mempty
                (aview l)
                (fieldNameToText fn)
                (invmap toShortText fromShortText unjsonDef)

    -- Likely not correct
    monoidalFieldAla fn _pack l =
        UnjsonFG $
            fmap unpack $
                fieldBy
                    (fieldNameToText fn)
                    (_pack . aview l)
                    (fieldNameToText fn)
                    myUnjsonDef

    prefixedFields fn l =
        UnjsonFG $
            fieldBy
                (fieldNameToText fn)
                (aview l)
                (fieldNameToText fn)
                unjsonDef

    knownField _fn =
        UnjsonFG $ pure ()

    hiddenField = id

    deprecatedSince _spec _msg =
        id

    removedIn _spec _msg =
        id

    availableSince _spec _a =
        id

    availableSinceWarn _spec =
        id

contramapFieldDef :: (b -> a) -> FieldDef a s -> FieldDef b s
contramapFieldDef f (FieldReqDef name doc ext d) = FieldReqDef name doc (ext . f) d
contramapFieldDef f (FieldOptDef name doc ext d) = FieldOptDef name doc (ext . f) d
contramapFieldDef f (FieldDefDef name doc def ext d) = FieldDefDef name doc def (ext . f) d
contramapFieldDef f (FieldRODef name doc ext d) = FieldRODef name doc (ext . f) d

unjsonPrettyParsec :: (Parsec a, Pretty a) => UnjsonDef a
unjsonPrettyParsec =
    unjsonInvmapR
        (either fail return . eitherParsec)
        prettyShow
        unjsonDef

instance (Typeable sep, Typeable b, MyContraint a) => MyContraint (List sep b a) where
    myUnjsonDef :: UnjsonDef (List sep b a)
    myUnjsonDef = invmap pack unpack (arrayOf myUnjsonDef)

instance (Typeable sep, Typeable a, Newtype a b, Sep sep, MyContraint b) => MyContraint (NonEmpty' sep b a) where
    myUnjsonDef
        :: ( Typeable sep
           , Typeable a
           , Newtype a b
           , Sep sep
           , MyContraint b
           )
        => UnjsonDef (NonEmpty' sep b a)
    myUnjsonDef =
        unjsonInvmapR
            (maybe (fail "nonEmpty") (pure . pack) . NE.nonEmpty . unpack)
            (NE.toList . unpack)
            $ arrayOf myUnjsonDef

instance (Typeable from, Typeable to) => MyContraint (SymbolicPath from to) where
    myUnjsonDef :: UnjsonDef (SymbolicPath from to)
    myUnjsonDef = unjsonPrettyParsec

instance MyContraint CompilerFlavor where
    myUnjsonDef = unjsonPrettyParsec

instance MyContraint VersionRange where
    myUnjsonDef = unjsonPrettyParsec

instance (MyContraint a, MyContraint b) => MyContraint (a, b) where
    myUnjsonDef = unjsonTuple2By myUnjsonDef myUnjsonDef

x :: UnjsonFG SourceRepoList SourceRepoList
x = sourceRepositoryPackageGrammar

unjsonTuple2By :: UnjsonDef k -> UnjsonDef v -> UnjsonDef (k, v)
unjsonTuple2By uk uv =
    unjsonInvmapR
        (\(vk, vv) -> (,) <$> parse uk vk <*> parse uv vv)
        (bimap (unjsonToJSON uk) (unjsonToJSON uv))
        unjsonDef
