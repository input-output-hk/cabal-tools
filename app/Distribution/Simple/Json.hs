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

import Control.Applicative.Free
import Data.Text qualified as T
import Distribution.Types.Json ()
import GHC.Generics

-- FIXME: selector names can be null
instance {-# OVERLAPPING #-} (Typeable c, Unjson c, Selector t) => GFieldDef (S1 t (K1 i (Flag c))) where
    gFieldDef = M1 . K1 <$> hoistAp (contramapFieldDef (flagToMaybe . unK1 . unM1)) (maybeToFlag <$> fieldOpt name id name)
      where
        name = T.pack $ selName (HProxy :: HProxy t f a)

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
