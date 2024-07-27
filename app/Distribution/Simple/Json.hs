module Distribution.Simple.Json where

import Data.Functor.Invariant
import Data.Typeable

import Distribution.Simple.Compiler
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup

-- Yes, this belongs here
import Distribution.Types.LocalBuildConfig

import Data.Aeson qualified as Aeson
import Unjson

import Distribution.Types.Json ()
import GHC.Generics

instance {-# OVERLAPPING #-} (Unjson a, Typeable a) => GFieldDef'K (K1 i (Flag a)) where
    gFieldDef'K _cn sn = dimapApFieldDef unK1 K1 $ dimapApFieldDef flagToMaybe maybeToFlag $ fieldOpt sn id sn

deriving via UnjsonShowRead PackageDB instance Unjson PackageDB

deriving via UnjsonObject Compiler instance Unjson Compiler

deriving via UnjsonObject CompilerId instance Unjson CompilerId

deriving via UnjsonShowRead (Maybe CompilerFlag) instance Unjson (Maybe CompilerFlag)

instance Unjson (Maybe PackageDB) where
    unjsonDef = invmap readPackageDb showPackageDb unjsonDef

-- deriving via UnjsonObject ConfigFlags instance Unjson ConfigFlags

deriving via UnjsonObject (InstallDirs a) instance (Unjson a, Typeable a) => Unjson (InstallDirs a)

deriving via UnjsonPrettyParsec HaddockTarget instance Unjson HaddockTarget

deriving via UnjsonPrettyParsec TestShowDetails instance Unjson TestShowDetails

deriving via UnjsonEnum DebugInfoLevel instance Unjson DebugInfoLevel

deriving via UnjsonEnum DumpBuildInfo instance Unjson DumpBuildInfo

deriving via UnjsonObject BuildOptions instance Unjson BuildOptions

instance Unjson PathTemplate where
    unjsonDef = invmap toPathTemplate fromPathTemplate unjsonDef

instance Unjson ProfDetailLevel where
    unjsonDef = unjsonGenericAeson "ProfDetailLevel" Aeson.defaultOptions
