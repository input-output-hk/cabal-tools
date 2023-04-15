{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Distribution.Client.DistDirLayout
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.NixStyleOptions
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup
import Distribution.Client.Types.RepoName
import Distribution.Compat.CharParsing qualified as P
import Distribution.Compat.Prelude
import Distribution.Package
import Distribution.Parsec
import Distribution.Pretty (prettyShow)
import Distribution.ReadE (parsecToReadE)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import Distribution.Simple.Utils
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.FilePath ((<.>), (</>))
import Text.Pretty.Simple (pPrint)

printCommandHelp :: (String -> String) -> IO ()
printCommandHelp help = getProgName >>= putStr . help

printErrors :: [String] -> IO ()
printErrors errs = do
  putStr (intercalate "\n" errs)
  exitWith (ExitFailure 1)

printOptionsList :: [String] -> IO ()
printOptionsList = putStr . unlines

printGlobalHelp :: (String -> String) -> IO ()
printGlobalHelp = printCommandHelp

newtype MakeNixPlanFlags = MakeNixPlanFlags
  { repoPaths :: Map.Map RepoName FilePath
  }
  deriving (Show)

defaultMakeNixPlanFlags :: MakeNixPlanFlags
defaultMakeNixPlanFlags = MakeNixPlanFlags {repoPaths = mempty}

main :: IO ()
main = do
  args <- getArgs

  let commands = [commandAddAction cmdMakeNixPlanUI cmdMakeNixPlanAction]

  case commandsRun (globalCommand commands) commands args of
    CommandHelp help -> printGlobalHelp help
    CommandList opts -> printOptionsList opts
    CommandErrors errs -> printErrors errs
    CommandReadyToGo (globalFlags, commandParse) ->
      case commandParse of
        CommandHelp help -> printCommandHelp help
        CommandList opts -> printOptionsList opts
        CommandErrors errs -> printErrors errs
        CommandReadyToGo action -> do
          action globalFlags

cmdMakeNixPlanUI :: CommandUI (NixStyleFlags MakeNixPlanFlags)
cmdMakeNixPlanUI =
  CommandUI
    { commandName = "make-nix-install-plan",
      commandSynopsis = "Makes an install-plan",
      commandUsage = ("Usage: " ++),
      commandDescription = Nothing,
      commandNotes = Nothing,
      commandDefaultFlags = defaultNixStyleFlags defaultMakeNixPlanFlags,
      commandOptions =
        nixStyleOptions
          ( const
              [ option
                  []
                  ["repo-path"]
                  "set repo paths"
                  repoPaths
                  (\v flags -> flags {repoPaths = v})
                  ( reqArg
                      "REPO-PATH"
                      ( parsecToReadE
                          (const "something-expected")
                          (fmap (uncurry Map.singleton) repoPathParser)
                      )
                      (map show . toList)
                  )
              ]
          )
    }
  where
    repoPathParser :: ParsecParser (RepoName, FilePath)
    repoPathParser =
      (,) <$> parsec <* P.char ':' <*> parsecFilePath

cmdMakeNixPlanAction :: NixStyleFlags MakeNixPlanFlags -> [String] -> GlobalFlags -> IO ()
cmdMakeNixPlanAction nixStyleFlags _extraArgs globalFlags = do
  let NixStyleFlags
        { configFlags = ConfigFlags {configVerbosity},
          extraFlags = MakeNixPlanFlags {repoPaths}
        } = nixStyleFlags

  let cliConfig = commandLineFlagsToProjectConfig globalFlags nixStyleFlags mempty
  let verbosity = fromFlagOrDefault Verbosity.normal configVerbosity

  ProjectBaseContext {distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  repareRepositories verbosity (projectConfigBuildOnly projectConfig) (projectConfigShared projectConfig) repoPaths

  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, totalIndexState, activeRepos) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  let nixPlanPath = distProjectCacheFile distDirLayout "nix"
  writeHaskellNixPlan verbosity (nixPlanPath </> "plan.nix") elaboratedPlan elaboratedSharedConfig totalIndexState activeRepos

repareRepositories :: Verbosity -> ProjectConfigBuildOnly -> ProjectConfigShared -> Map.Map RepoName FilePath -> IO ()
repareRepositories verbosity ProjectConfigBuildOnly {projectConfigCacheDir} ProjectConfigShared {projectConfigRemoteRepos} repoPaths = do
  -- AFAIU the code, this flag is always set (because it is set in the initial config)
  -- note this can be overrideen with --remote-repo-cache
  let cacheDir = fromFlagOrDefault (error "unknown projectConfigCacheDir") projectConfigCacheDir

  for_ (Map.toList repoPaths) $ \(repoName, storePath) -> do
    let repoCacheDir = cacheDir </> unRepoName repoName
    repoCacheDirExists <- doesDirectoryExist repoCacheDir
    when repoCacheDirExists $ do
      warn verbosity $ "Directory " <> repoCacheDir <> " already exists"
      exitFailure
    pPrint projectConfigRemoteRepos
    pPrint projectConfigCacheDir
    pPrint repoPaths

writeHaskellNixPlan :: Verbosity -> FilePath -> ElaboratedInstallPlan -> ElaboratedSharedConfig -> TotalIndexState -> ActiveRepos -> IO ()
writeHaskellNixPlan verbosity outputDir elaboratedPlan _elaboratedSharedConfig _totalIndexState _activeRepos = do
  createDirectoryIfMissing True outputDir
  let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]
  for_ ecps $
    \ElaboratedConfiguredPackage
       { elabPkgSourceId,
         elabPkgDescriptionOverride
       } -> do
        let pkgFile = outputDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
        for_ elabPkgDescriptionOverride $ \pkgTxt -> do
          info verbosity $ "Writing package description for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
          BSL.writeFile pkgFile pkgTxt
