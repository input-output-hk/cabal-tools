{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Foldable (for_)
import Distribution.Client.GlobalFlags
import Distribution.Client.InstallPlan (toList)
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning (rebuildInstallPlan)
import Distribution.Client.Setup (ConfigFlags (..))
import Distribution.Simple.Command
import Distribution.Simple.Flag
import Distribution.Verbosity
import qualified Distribution.Verbosity as Verbosity
import System.Environment
import Text.Pretty.Simple (CheckColorTty (NoCheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsDarkBg, pPrintOpt)

pPrint' :: (Show a) => a -> IO ()
pPrint' = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True, outputOptionsCompactParens = True}

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

    (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, totalIndexState, activeRepos) <-
        rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing

    putStrLn "-------------------- projectConfig --------------------"
    pPrint' projectConfig

    putStrLn "-------------------- localPackages --------------------"
    pPrint' localPackages

    putStrLn "-------------------- elaboratedInstallPlan --------------------"
    for_ (toList elaboratedPlan) pPrint'

    -- NOTE: the improved plan is really just like elaborated plan with cached
    -- packages marked as pre-installed

    -- putStrLn "-------------------- elaboratedInstallPlan --------------------"
    -- for_ (toList improvedPlan) pPrint'

    putStrLn "-------------------- elaboratedSharedConfig --------------------"
    pPrint' elaboratedSharedConfig

    putStrLn "-------------------- totalIndexState --------------------"
    pPrint' totalIndexState

    putStrLn "-------------------- activeRepos --------------------"
    pPrint' activeRepos
