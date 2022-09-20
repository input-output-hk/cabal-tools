{-# LANGUAGE ApplicativeDo #-}

import Control.Monad (join)
import Debug.RecoverRTTI (anythingToString)
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( defaultCabalDirLayout,
    defaultDistDirLayout,
  )
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig (findProjectRoot)
import Distribution.Client.ProjectPlanning
  ( rebuildInstallPlan,
    rebuildProjectConfig,
  )
import Distribution.Parsec (eitherParsec)
import Distribution.Verbosity (Verbosity, moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import Options.Applicative
import Text.Pretty.Simple (pPrintString)

main :: IO ()
main =
  join $
    execParser $
      info
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

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      -- more verbose here to list the project files which have affected
      -- the project configuration with no extra options
      (moreVerbose verbosity)
      httpTransport
      distDirLayout
      mempty

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  pPrintString $ anythingToString elaboratedPlan

  pPrintString $ anythingToString elaboratedSharedConfig
