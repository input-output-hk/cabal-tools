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
import Distribution.Client.Setup (globalCommand)
import Distribution.Client.Setup hiding (globalCommand)
import Distribution.Client.Types.Repo (RemoteRepo (..))
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
import Distribution.Utils.NubList (fromNubList)
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import Hackage.Security.Client qualified as Sec
import Hackage.Security.Client.Formats qualified as Sec
import Hackage.Security.Client.Repository (DownloadedFile)
import Hackage.Security.Client.Repository qualified as Sec
import Hackage.Security.Client.Repository.Cache qualified as Sec
import Hackage.Security.Util.Path qualified as Sec
import System.Directory (createDirectoryIfMissing, createFileLink, doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.FilePath ((<.>), (</>))

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
                  ["remote-repo-path"]
                  "Set a path for a remote repository"
                  repoPaths
                  (\v flags -> flags {repoPaths = v})
                  ( reqArg
                      "REPO=PATH"
                      ( parsecToReadE
                          (const "expected an argument of the form repository-name=path")
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

  repareRemoteRepositories verbosity (projectConfigBuildOnly projectConfig) (projectConfigShared projectConfig) repoPaths

  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, totalIndexState, activeRepos) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  let nixPlanPath = distProjectCacheFile distDirLayout "nix"
  writeHaskellNixPlan verbosity (nixPlanPath </> "plan.nix") elaboratedPlan elaboratedSharedConfig totalIndexState activeRepos

repareRemoteRepositories :: Verbosity -> ProjectConfigBuildOnly -> ProjectConfigShared -> Map.Map RepoName FilePath -> IO ()
repareRemoteRepositories
  verbosity
  ProjectConfigBuildOnly {projectConfigCacheDir}
  ProjectConfigShared {projectConfigRemoteRepos}
  repoPaths = do
    -- AFAIU the code, this flag is always set (because it is set in the initial config)
    -- note this can be overridden with --remote-repo-cache
    let sharedCacheDir = fromFlagOrDefault (error "unknown projectConfigCacheDir") projectConfigCacheDir

    sharedCacheDirExists <- doesDirectoryExist sharedCacheDir
    when sharedCacheDirExists $ do
      isEmpty <- null <$> listDirectory sharedCacheDir
      unless isEmpty $ do
        notice verbosity $
          unwords
            [ "Directory " <> sharedCacheDir <> " already exists and is not empty. I refuse to mess it up.",
              "You can use the --remote-repo-cache global flag to use another directory."
            ]
        exitFailure

    for_ (fromNubList projectConfigRemoteRepos) $ \RemoteRepo {remoteRepoName} -> do
      let repoLocalDir = sharedCacheDir </> unRepoName remoteRepoName
      case Map.lookup remoteRepoName repoPaths of
        Nothing -> do
          notice verbosity $
            "Project uses a repository named " <> prettyShow remoteRepoName <> " for which we do not have a path."
          exitFailure
        Just path -> do
          notice verbosity $
            "Project uses a repository named " <> prettyShow remoteRepoName <> " for which we have a path."

          createDirectoryIfMissingVerbose verbosity False repoLocalDir

          cachePath <- Sec.makeAbsolute $ Sec.fromFilePath repoLocalDir
          let cache = Sec.Cache cachePath cacheLayout

          timestampJson <- mkLinkedLocalFile (path </> "timestamp.json")
          Sec.cacheRemoteFile cache timestampJson Sec.FUn (Sec.CacheAs Sec.CachedTimestamp)

          rootJson <- mkLinkedLocalFile (path </> "root.json")
          Sec.cacheRemoteFile cache rootJson Sec.FUn (Sec.CacheAs Sec.CachedRoot)

          snapshotJson <- mkLinkedLocalFile (path </> "snapshot.json")
          Sec.cacheRemoteFile cache snapshotJson Sec.FUn (Sec.CacheAs Sec.CachedSnapshot)

          mirrorsJson <- mkLinkedLocalFile (path </> "mirrors.json")
          Sec.cacheRemoteFile cache mirrorsJson Sec.FUn (Sec.CacheAs Sec.CachedMirrors)

          indexTarGz <- mkLinkedLocalFile (path </> "01-index.tar.gz")
          Sec.cacheRemoteFile cache indexTarGz Sec.FGz Sec.CacheIndex

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

mkLinkedLocalFile :: FilePath -> IO (LinkedLocalFile a)
mkLinkedLocalFile fp = LinkedLocalFile <$> Sec.makeAbsolute (Sec.fromFilePath fp)

newtype LinkedLocalFile a = LinkedLocalFile (Sec.Path Sec.Absolute)

instance DownloadedFile LinkedLocalFile where
  downloadedVerify (LinkedLocalFile _fp) _trustedInfo =
    return True

  downloadedCopyTo (LinkedLocalFile local) dst = do
    srcA <- Sec.toAbsoluteFilePath local
    dstA <- Sec.toAbsoluteFilePath dst
    createFileLink srcA dstA

  downloadedRead (LinkedLocalFile local) =
    Sec.readLazyByteString local

cacheLayout :: Sec.CacheLayout
cacheLayout =
  Sec.cabalCacheLayout
    { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar",
      Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx",
      Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
    }
  where
    cacheFn = Sec.rootPath . Sec.fragment
