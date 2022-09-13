{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( defaultCabalDirLayout,
    defaultDistDirLayout,
  )
import Distribution.Client.HttpUtils (configureTransport)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectConfig (findProjectRoot)
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage
      ( ElaboratedConfiguredPackage,
        elabLocalToProject,
        elabPkgDescriptionOverride,
        elabPkgSourceId
      ),
    rebuildInstallPlan,
    rebuildProjectConfig,
  )
import Distribution.Package (pkgName)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import qualified Distribution.Simple.Utils as Cabal
import Distribution.Verbosity (Verbosity, moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import Options.Applicative
import System.FilePath ((<.>), (</>))

optionsParser :: Parser Options
optionsParser =
  Options
    <$> option
      (eitherReader eitherParsec)
      ( long "verbosity"
          <> metavar "VERBOSITY"
          <> value Verbosity.normal
          <> help "Verbosity"
      )
    <*> optional (argument str (metavar "INPUT-DIR"))
    <*> argument str (metavar "OUTPUT-DIR" <> value "./out")

main :: IO ()
main = execParser opts >>= doMain
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

data Options = Options
  { verbosity :: Verbosity,
    inputDir :: Maybe FilePath,
    outputDir :: FilePath
  }

doMain :: Options -> IO ()
doMain Options {inputDir, outputDir, verbosity} = do
  cabalDir <- getCabalDir
  let cabalDirLayout = defaultCabalDirLayout cabalDir

  Right projectRoot <- findProjectRoot inputDir Nothing
  let distDirLayout = defaultDistDirLayout projectRoot (Just outputDir)

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
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

  putStrLn $ "Writing detailed plan to " ++ outputDir

  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

  let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]

  for_ ecps $
    \ElaboratedConfiguredPackage
       { elabPkgSourceId,
         elabPkgDescriptionOverride
       } -> do
        let pkgFile = outputDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
        for_ elabPkgDescriptionOverride $ \pkgTxt -> do
          Cabal.info verbosity $ "Writing package description for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
          BSL.writeFile pkgFile pkgTxt
