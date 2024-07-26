module Distribution.Json where

import Data.Typeable

import Data.Map

import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.System

import Unjson

deriving via UnjsonPrettyParsec CompilerFlavor instance Unjson CompilerFlavor

deriving via UnjsonPrettyParsec Platform instance Unjson Platform

deriving via UnjsonPrettyParsec ModuleName instance Unjson ModuleName

instance (Typeable a, Unjson a) => Unjson (Map ModuleName a) where
    unjsonDef = mapOfBy

deriving via UnjsonShowRead CabalSpecVersion instance Unjson CabalSpecVersion

instance (Typeable v, Unjson v) => Unjson (PerCompilerFlavor v) where
    unjsonDef = objectOf genericFieldDef
