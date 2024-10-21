module Main (main) where

import Data.Aeson.Text qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text.Lazy.IO qualified as TL
import Distribution.Client.GlobalFlags
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.ProjectConfig (ProjectConfig, commandLineFlagsToProjectConfig)
import Distribution.Client.ProjectOrchestration (CurrentCommand (..), ProjectBaseContext (..), establishProjectBaseContext)
import Distribution.Client.ProjectPlanning (rebuildInstallPlan)
import Distribution.Simple.Command (CommandParse (..), CommandUI (..), commandParseArgs)
import Distribution.Simple.Setup (
    ConfigFlags (..),
    fromFlagOrDefault,
 )
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import System.Environment
import Unjson

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
