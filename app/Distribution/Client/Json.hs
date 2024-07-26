{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Distribution.Client.Json where

import Data.Functor.Invariant
import Data.Typeable
import GHC.Generics

import Distribution.Client.InstallPlan
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types

import Generic.Data
import Unjson

import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Map

import Distribution.Client.HashValue
import Distribution.Client.IndexUtils.ActiveRepos
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Types.SourceRepo

import Distribution.Backpack.Json ()
import Distribution.Json ()
import Distribution.License.Json ()
import Distribution.Simple.Json ()
import Distribution.Solver.Json ()
import Distribution.Types.Json ()

import Network.Json ()

deriving via UnjsonEnum NotPerComponentBuildType instance Unjson NotPerComponentBuildType
deriving via Generically NotPerComponentBuildType instance Enum NotPerComponentBuildType
deriving via Generically NotPerComponentBuildType instance Bounded NotPerComponentBuildType

deriving via UnjsonEnum NotPerComponentReason instance Unjson NotPerComponentReason
deriving via (FiniteEnumeration NotPerComponentReason) instance Enum NotPerComponentReason
deriving via (FiniteEnumeration NotPerComponentReason) instance Bounded NotPerComponentReason

deriving via UnjsonEnum BuildStyle instance Unjson BuildStyle
deriving via (FiniteEnumeration BuildStyle) instance Enum BuildStyle
deriving via (FiniteEnumeration BuildStyle) instance Bounded BuildStyle

deriving via UnjsonEnum MemoryOrDisk instance Unjson MemoryOrDisk
deriving via Generically MemoryOrDisk instance Enum MemoryOrDisk
deriving via Generically MemoryOrDisk instance Bounded MemoryOrDisk

deriving via Generically SetupScriptStyle instance Enum SetupScriptStyle
deriving via Generically SetupScriptStyle instance Bounded SetupScriptStyle
deriving via UnjsonEnum SetupScriptStyle instance Unjson SetupScriptStyle

deriving via UnjsonObject ElaboratedComponent instance Unjson ElaboratedComponent

deriving via UnjsonObject ElaboratedConfiguredPackage instance Unjson ElaboratedConfiguredPackage

deriving via UnjsonObject ElaboratedPackage instance Unjson ElaboratedPackage

-- deriving via UnjsonObject ElaboratedSharedConfig instance Unjson ElaboratedSharedConfig

instance Unjson ProjectConfigProvenance where
    unjsonDef =
        disjointUnionOf
            "ProjectConfigProvenance"
            [
                ( "ProjectConfigProvenance"
                , isConstrByName "ProjectConfigProvenance"
                , pure Implicit
                )
            ,
                ( "ProjectConfigProvenance"
                , isConstrByName "ProjectConfigProvenance"
                , pure Explicit <*> field "projectConfigPath" (\case ~(Explicit path) -> path) "projectConfigPath"
                )
            ]

-- "Implicit" "Implicit" Implicit (\case {})
-- , gConst "Explicit" "Explicit" Explicit (\case ~(Explicit pkg) -> pkg)
-- ]

-- deriving via UnjsonObject ProjectConfig instance Unjson ProjectConfig
--
-- deriving via UnjsonObject ProjectConfigShared instance Unjson ProjectConfigShared
--
-- deriving via UnjsonObject ProjectConfigBuildOnly instance Unjson ProjectConfigBuildOnly

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

-- | TODO: Fixing SubComponentTarget = WholeComponent for the moment
instance Unjson ComponentTarget where
    unjsonDef =
        invmap
            (`ComponentTarget` WholeComponent)
            (\(ComponentTarget cn _) -> cn)
            unjsonDef

instance (Unjson a, Typeable a) => Unjson (PackageLocation (Maybe a)) where
    unjsonDef =
        disjointUnionOf
            "PackageLocation"
            [
                ( "LocalUnpackedPackage"
                , isConstrByName "LocalUnpackedPackage"
                , LocalUnpackedPackage
                    <$> field
                        "path"
                        (\case ~(LocalUnpackedPackage fp) -> fp)
                        "path"
                )
            ,
                ( "LocalTarballPackage"
                , isConstrByName "LocalTarballPackage"
                , LocalTarballPackage
                    <$> field
                        "path"
                        (\case ~(LocalTarballPackage fp) -> fp)
                        "path"
                )
            ,
                ( "RemoteTarballPackage"
                , isConstrByName "RemoteTarballPackage"
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
                , isConstrByName "RepoTarballPackage"
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
                , isConstrByName "RemoteSourceRepoPackage"
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

deriving via UnjsonObject (SourceRepositoryPackage Maybe) instance Unjson (SourceRepositoryPackage Maybe)

deriving via UnjsonObject (SourceRepositoryPackage []) instance Unjson (SourceRepositoryPackage [])

instance (Typeable a, Unjson a) => Unjson (Map RepoName a) where
    unjsonDef = mapOfBy

deriving via UnjsonObject TotalIndexState instance Unjson TotalIndexState

deriving via UnjsonPrettyParsec ActiveRepos instance Unjson ActiveRepos

deriving via UnjsonPrettyParsec LocalRepo instance Unjson LocalRepo

deriving via UnjsonPrettyParsec RemoteRepo instance Unjson RemoteRepo

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

deriving via UnjsonPrettyParsec RepoName instance Unjson RepoName

deriving via UnjsonPrettyParsec RepoIndexState instance Unjson RepoIndexState

deriving via Map a b instance Unjson (Map a b) => Unjson (MapMappend a b)

deriving via UnjsonObject ConfiguredId instance Unjson ConfiguredId

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
