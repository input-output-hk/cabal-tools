{-# LANGUAGE ApplicativeDo #-}

import Control.Monad.State as State
import Data.Maybe (fromMaybe)
import Distribution.Client.Config
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanOutput
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Flag (Flag (NoFlag))
import Distribution.Simple.GHC qualified as Cabal
import Distribution.Simple.Program (emptyProgramDb)
import Distribution.System (Platform (Platform), buildPlatform)
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import My.ProjectPlanning qualified as My (rebuildInstallPlan, rebuildProjectConfig)
import Options.Applicative

main :: IO ()
main =
  join $
    execParser $
      Options.Applicative.info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Extracts a cabal install plan")
  where
    optionsParser = do
      verbosity <-
        option
          (eitherReader eitherParsec)
          ( long "verbosity"
              <> metavar "VERBOSITY"
              <> value Verbosity.normal
              <> help "Verbosity"
          )
      inputDir <- optional (argument str (metavar "INPUT-DIR"))
      outputDir <- argument str (metavar "OUTPUT-DIR" <> value "./out")
      pure $ doMain verbosity inputDir outputDir

doMain :: Verbosity -> Maybe FilePath -> [Char] -> IO ()
doMain verbosity inputDir outputDir = do
  cabalDir <- getCabalDir
  let cabalDirLayout = defaultCabalDirLayout cabalDir

  Right projectRoot <- findProjectRoot inputDir Nothing
  let distDirLayout = defaultDistDirLayout projectRoot (Just outputDir)

  httpTransport <- configureTransport verbosity mempty Nothing

  (compiler, mPlatform, _) <- Cabal.configure verbosity Nothing Nothing emptyProgramDb
  let (Platform arch os) = fromMaybe buildPlatform mPlatform

  putStrLn "now rebuilding project configuration"

  (projectConfig, localPackages) <-
    My.rebuildProjectConfig
      verbosity
      compiler
      arch
      os
      httpTransport
      distDirLayout
      NoFlag -- ignoreProject
      NoFlag -- configFile

  putStrLn "now rebuilding the install plan"

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    My.rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  putStrLn $ "Writing detailed plan to " ++ outputDir

  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig
