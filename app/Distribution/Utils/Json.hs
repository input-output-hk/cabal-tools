module Distribution.Utils.Json where

import Data.Typeable

import Data.Functor.Invariant

import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Utils.ShortText

import Unjson

instance Unjson ShortText where
    unjsonDef = invmap toShortText fromShortText unjsonDef

deriving via UnjsonPrettyParsec (SymbolicPath a b) instance Unjson (SymbolicPath a b)

instance (Unjson a, Ord a, Typeable a) => Unjson (NubList a) where
    unjsonDef :: UnjsonDef (NubList a)
    unjsonDef = invmap toNubList fromNubList unjsonDef
