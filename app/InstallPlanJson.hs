{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
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
import Data.Aeson.Text qualified as Aeson
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
import Data.Text.Lazy.IO qualified as TL
import Data.Traversable (for)
import Data.Tuple (swap)
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
import Distribution.Backpack.ConfiguredComponent ()
import Distribution.Backpack.ModuleShape (ModuleShape (..))
import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Client.FileMonitor (MonitorStateFileSet)
import Distribution.Client.GlobalFlags
import Distribution.Client.HashValue (HashValue, showHashValue)
import Distribution.Client.IndexUtils (ActiveRepos, RepoIndexState, TotalIndexState)
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.PackageHash ()
import Distribution.Client.ProjectConfig (MapMappend (..), ProjectConfig, commandLineFlagsToProjectConfig)
import Distribution.Client.ProjectOrchestration (CurrentCommand (..), ProjectBaseContext (..), establishProjectBaseContext)
import Distribution.Client.ProjectPlanning (rebuildInstallPlan)
import Distribution.Client.Types (
    ConfiguredId (..),
    LocalRepo,
    PackageLocation (..),
    PackageSpecifier,
    RemoteRepo,
    Repo (RepoLocalNoIndex, RepoRemote, RepoSecure),
    UnresolvedSourcePackage,
 )
import Distribution.Client.Types.RepoName
import Distribution.Client.Types.SourceRepo (
    SourceRepositoryPackage (..),
 )
import Distribution.Compat.Newtype ()
import Distribution.License qualified
import Distribution.PackageDescription
import Distribution.Parsec (
    Parsec (..),
    eitherParsec,
    explicitEitherParsec,
 )
import Distribution.Pretty (Pretty, prettyShow)
import Distribution.SPDX.License qualified
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
import Distribution.System
import Distribution.Utils.Path
import Distribution.Utils.ShortText (ShortText, fromShortText, toShortText)
import Distribution.Utils.Structured (Structured, Tag)
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
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
import Text.Pretty.Simple
import Text.Read (readMaybe)
import Unjson

import Data.Typeable
import Distribution.Client.Json ()
import Distribution.Json ()
import Distribution.License.Json ()
import Distribution.Types.Json ()

--
-- Instances
--

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
                [ "elaboratedPlan" Aeson..= map (unjsonToJSON unjsonDef) (InstallPlan.toList elaboratedPlan)
                , "totalIndexState" Aeson..= unjsonToJSON unjsonDef totalIndexState
                , "activeRepos" Aeson..= unjsonToJSON unjsonDef activeRepos
                ]

printJson :: Unjson a => a -> IO ()
printJson = BL.putStr . unjsonToByteStringLazy' (Options{pretty = True, indent = 2, nulls = True}) unjsonDef
