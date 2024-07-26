{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Distribution.Types.Json where

import Data.Map
import Data.Typeable

import Distribution.Types.AbiHash
import Distribution.Types.ComponentId
import Distribution.Types.ComponentName
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Flag
import Distribution.Types.InstalledPackageInfo
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.Module
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersion
import Distribution.Types.SourceRepo
import Distribution.Types.UnitId

import Distribution.Compiler
import Distribution.Simple.Compiler

import Unjson

import Distribution.Backpack.Json ()
import Distribution.Json ()
import Distribution.License.Json ()
import Distribution.Types.Benchmark
import Distribution.Types.BenchmarkInterface
import Distribution.Types.BuildInfo
import Distribution.Types.BuildType
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.Executable
import Distribution.Types.ExecutableScope
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.LegacyExeDependency
import Distribution.Types.Library
import Distribution.Types.Mixin
import Distribution.Types.ModuleReexport
import Distribution.Types.SetupBuildInfo
import Distribution.Types.TestSuite
import Distribution.Types.TestSuiteInterface
import Distribution.Types.TestType
import Distribution.Types.UnqualComponentName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.Utils.Json ()
import Generic.Data
import Language.Haskell.Extension

deriving via UnjsonPrettyParsec FlagAssignment instance Unjson FlagAssignment

deriving via UnjsonEnum OptimisationLevel instance Unjson OptimisationLevel

deriving via UnjsonShowRead ComponentRequestedSpec instance Unjson ComponentRequestedSpec

deriving via UnjsonPrettyParsec AbiHash instance Unjson AbiHash

deriving via UnjsonPrettyParsec AbiTag instance Unjson AbiTag

deriving via UnjsonPrettyParsec AbiDependency instance Unjson AbiDependency

deriving via UnjsonPrettyParsec ExposedModule instance Unjson ExposedModule

deriving via UnjsonPrettyParsec ComponentName instance Unjson ComponentName

deriving via UnjsonPrettyParsec UnitId instance Unjson UnitId

deriving via UnjsonEnum LibraryVisibility instance Unjson LibraryVisibility

deriving via Generically LibraryVisibility instance Enum LibraryVisibility

deriving via Generically LibraryVisibility instance Bounded LibraryVisibility

-- | FIXME
deriving via UnjsonShowRead LibraryName instance Unjson LibraryName

deriving via UnjsonObject InstalledPackageInfo instance Unjson InstalledPackageInfo

deriving via UnjsonPrettyParsec ComponentId instance Unjson ComponentId

deriving via UnjsonPrettyParsec PackageId instance Unjson PackageId

deriving via UnjsonPrettyParsec PackageName instance Unjson PackageName

deriving via UnjsonObject PackageDescription instance Unjson PackageDescription

instance (Typeable a, Unjson a) => Unjson (Map PackageName a) where
    unjsonDef = mapOfBy

deriving via UnjsonPrettyParsec Module instance Unjson Module

deriving via UnjsonPrettyParsec PackageVersionConstraint instance Unjson PackageVersionConstraint

deriving via UnjsonPrettyParsec PkgconfigDependency instance Unjson PkgconfigDependency

deriving via UnjsonPrettyParsec PkgconfigName instance Unjson PkgconfigName

deriving via UnjsonPrettyParsec PkgconfigVersion instance Unjson PkgconfigVersion

deriving via UnjsonPrettyParsec RepoKind instance Unjson RepoKind

deriving via UnjsonPrettyParsec RepoType instance Unjson RepoType

deriving via UnjsonObject SourceRepo instance Unjson SourceRepo

instance {-# INCOHERENT #-} Unjson (PkgconfigName, Maybe PkgconfigVersion) where
    unjsonDef = objectOf genericFieldDef

deriving via UnjsonPrettyParsec VersionRange instance Unjson VersionRange

deriving via UnjsonPrettyParsec BuildType instance Unjson BuildType

deriving via UnjsonObject SetupBuildInfo instance Unjson SetupBuildInfo

deriving via UnjsonPrettyParsec Dependency instance Unjson Dependency

deriving via UnjsonPrettyParsec ExeDependency instance Unjson ExeDependency

deriving via UnjsonPrettyParsec UnqualComponentName instance Unjson UnqualComponentName

deriving via UnjsonPrettyParsec ExecutableScope instance Unjson ExecutableScope

deriving via UnjsonPrettyParsec Extension instance Unjson Extension

deriving via UnjsonPrettyParsec ForeignLibOption instance Unjson ForeignLibOption

deriving via UnjsonPrettyParsec ForeignLibType instance Unjson ForeignLibType

deriving via UnjsonObject Benchmark instance Unjson Benchmark

deriving via UnjsonShowRead BenchmarkInterface instance Unjson BenchmarkInterface

deriving via UnjsonPrettyParsec Language instance Unjson Language

deriving via UnjsonObject BuildInfo instance Unjson BuildInfo

-- deriving via UnjsonObject PackageBuildDescr instance Unjson PackageBuildDescr

deriving via UnjsonPrettyParsec Mixin instance Unjson Mixin

deriving via UnjsonObject Executable instance Unjson Executable

deriving via UnjsonPrettyParsec Version instance Unjson Version

deriving via UnjsonObject ForeignLib instance Unjson ForeignLib

deriving via UnjsonPrettyParsec LegacyExeDependency instance Unjson LegacyExeDependency

deriving via UnjsonPrettyParsec LibVersionInfo instance Unjson LibVersionInfo

deriving via UnjsonObject Library instance Unjson Library

deriving via UnjsonObject ModuleReexport instance Unjson ModuleReexport

deriving via UnjsonPrettyParsec TestType instance Unjson TestType

deriving via UnjsonObject TestSuite instance Unjson TestSuite

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
