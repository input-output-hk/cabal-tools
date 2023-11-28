{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Control.Exception
import Control.Monad.IO.Class
import Data.Foldable (for_, toList)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Distribution.Client.CmdBuild qualified as CmdBuild
import Distribution.Client.CmdErrorMessages
import Distribution.Client.Config
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Client.ProjectOrchestration hiding (establishProjectBaseContext, establishProjectBaseContextWithRoot)
import Distribution.Client.ProjectPlanning hiding (rebuildProjectConfig)
import Distribution.Client.RebuildMonad
import Distribution.Client.TargetProblem
import Distribution.Client.Targets
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.PackageSpecifier
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.PackageDescription qualified as PD
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (flagToMaybe, toFlag)
import Distribution.Simple.Setup qualified as Setup
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.Solver.Types.ConstraintSource
import Distribution.System
import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.NubList
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import Options.Applicative
import System.Environment (getArgs)
import System.Process (callProcess)

data Args = Args
    { argNamedPackages :: [PackageVersionConstraint]
    , argVerbosity :: Verbosity
    , argDryRun :: Bool
    , argConstraints :: [UserConstraint]
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> some (argument (maybeReader simpleParsec) (help "The packages to build"))
        <*> option (maybeReader simpleParsec) (long "verbosity" <> short 'v' <> value Verbosity.normal <> help "Verbosity")
        <*> switch (long "dry-run" <> help "Only compute a build plan")
        <*> many (option (maybeReader simpleParsec) (long "constraint" <> help "Constraints"))

main :: IO ()
main = do
    args <- getArgs
    case args of
        "act-as-setup" : _ ->
            callProcess "cabal" args
        _otherwise ->
            build =<< execParser opts
  where
    opts =
        info
            (argsParser <**> helper)
            ( fullDesc
                <> progDesc "Builds a package"
                <> header "hello - a busy package builder"
            )

build :: Args -> IO ()
build Args{argNamedPackages, argVerbosity, argDryRun, argConstraints} = do
    let cliConfig =
            mempty
                { projectPackagesNamed = argNamedPackages
                , projectConfigBuildOnly = mempty{projectConfigDryRun = toFlag argDryRun}
                , projectConfigShared =
                    mempty
                        { projectConfigConstraints = map (,ConstraintSourceCommandlineFlag) argConstraints
                        }
                }

    projectRoot <- either throwIO return =<< findProjectRoot Nothing Nothing

    baseCtx <- establishProjectBaseContextWithRoot argVerbosity cliConfig projectRoot OtherCommand

    buildCtx <- runProjectPreBuildPhase argVerbosity baseCtx $ \elaboratedPlan -> do
        let targetSelectors = [TargetPackageNamed pkgName Nothing | PackageVersionConstraint pkgName _ <- argNamedPackages]

        targets <-
            either (reportBuildTargetProblems argVerbosity) return $
                resolveTargets
                    CmdBuild.selectPackageTargets
                    CmdBuild.selectComponentTarget
                    elaboratedPlan
                    Nothing
                    targetSelectors

        for_ (Map.toList targets) $ \(unitId, cts) ->
            putStrLn $
                prettyShow unitId
                    <> " "
                    <> intercalate "," [prettyShow cn <> " " <> show sct <> ", selected by: " ++ intercalate "," (map showTargetSelector $ toList ts) | (ComponentTarget cn sct, ts) <- cts]

        return (elaboratedPlan, targets)

    printPlan argVerbosity baseCtx buildCtx

    buildOutcomes <- runProjectBuildPhase argVerbosity baseCtx buildCtx

    runProjectPostBuildPhase argVerbosity baseCtx buildCtx buildOutcomes

    putStrLn "done."

-- Copied the following from Distribution.Client.ProjectPlanning and
-- modified to always rebuild (because cabal logic does not take cli config
-- into account)

establishProjectBaseContextWithRoot
    :: Verbosity
    -> ProjectConfig
    -> ProjectRoot
    -> CurrentCommand
    -> IO ProjectBaseContext
establishProjectBaseContextWithRoot verbosity cliConfig projectRoot currentCommand = do
    cabalDir <- getCabalDir

    let mdistDirectory = Setup.flagToMaybe (projectConfigDistDir $ projectConfigShared cliConfig)
    let distDirLayout = defaultDistDirLayout projectRoot mdistDirectory

    httpTransport <-
        configureTransport
            verbosity
            (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
            (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)

    (projectConfig, localPackages) <-
        rebuildProjectConfig
            verbosity
            httpTransport
            distDirLayout
            cliConfig

    let mlogsDir = Setup.flagToMaybe (projectConfigLogsDir $ projectConfigBuildOnly projectConfig)

    mstoreDir <- traverse makeAbsolute $ Setup.flagToMaybe (projectConfigStoreDir $ projectConfigShared projectConfig)

    let cabalDirLayout = mkCabalDirLayout cabalDir mstoreDir mlogsDir

    let buildSettings = resolveBuildTimeSettings verbosity cabalDirLayout projectConfig

    return
        ProjectBaseContext
            { distDirLayout
            , cabalDirLayout
            , projectConfig
            , localPackages
            , buildSettings
            , currentCommand
            }

rebuildProjectConfig
    :: Verbosity
    -> HttpTransport
    -> DistDirLayout
    -> ProjectConfig
    -> IO
        ( ProjectConfig
        , [PackageSpecifier UnresolvedSourcePackage]
        )
rebuildProjectConfig
    verbosity
    httpTransport
    distDirLayout@DistDirLayout{distProjectRootDirectory, distProjectCacheDirectory}
    cliConfig =
        do
            (projectConfig, localPackages) <-
                runRebuild distProjectRootDirectory $ do
                    liftIO $ createDirectoryIfMissingVerbose verbosity True distProjectCacheDirectory

                    projectConfigSkeleton <-
                        readProjectConfig
                            verbosity
                            httpTransport
                            (projectConfigIgnoreProject $ projectConfigShared cliConfig)
                            (projectConfigConfigFile $ projectConfigShared cliConfig)
                            distDirLayout

                    let (projectConfig', _projectConfigImports) = PD.ignoreConditions projectConfigSkeleton

                    (compiler, Platform arch os, _) <- configureCompiler verbosity distDirLayout (projectConfig' <> cliConfig)

                    let projectConfig = instantiateProjectConfigSkeleton os arch (compilerInfo compiler) mempty projectConfigSkeleton

                    localPackages <- readLocalPackages verbosity distDirLayout (projectConfig <> cliConfig)

                    return (projectConfig, localPackages)

            putStrLn $
                unlines $
                    ("this build was affected by the following (project) config files:" :) $
                        [ "- " ++ path
                        | Explicit path <- toList $ projectConfigProvenance projectConfig
                        ]

            return (projectConfig <> cliConfig, localPackages)

readLocalPackages
    :: Verbosity
    -> DistDirLayout
    -> ProjectConfig
    -> Rebuild [PackageSpecifier UnresolvedSourcePackage]
readLocalPackages verbosity distDirLayout projectConfig = do
    pkgLocations <- findProjectPackages distDirLayout projectConfig

    liftIO $ do
        createDirectoryIfMissingVerbose verbosity True (distDirectory distDirLayout)
        createDirectoryIfMissingVerbose verbosity True (distProjectCacheDirectory distDirLayout)

    fetchAndReadSourcePackages
        verbosity
        distDirLayout
        (projectConfigShared projectConfig)
        (projectConfigBuildOnly projectConfig)
        pkgLocations

reportBuildTargetProblems :: Verbosity -> [TargetProblem'] -> IO a
reportBuildTargetProblems verbosity =
    reportTargetProblems verbosity "build"
