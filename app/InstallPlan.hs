{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (join)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_)
import Distribution.Client.Compat.Prelude (exitSuccess)
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanOutput
import Distribution.Client.ProjectPlanning
import Distribution.Compat.Directory qualified as Cabal
import Distribution.Package
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Utils qualified as Cabal
import Distribution.Verbosity (Verbosity, moreVerbose)
import Distribution.Verbosity qualified as Verbosity
import Network.URI
import Options.Applicative
import System.Directory (createFileLink)
import System.FilePath ((<.>), (</>))
import Text.Pretty.Simple (pPrint)

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
  Cabal.withTempDirectory verbosity "/tmp" "cabal-dir" $ \cabalDir -> do
    putStrLn $ "Using temp cabal-dir: " <> cabalDir

    let cabalDirLayout = defaultCabalDirLayout cabalDir

    Right projectRoot <- findProjectRoot inputDir Nothing

    absoluteOutputDir <- Cabal.makeAbsolute outputDir
    let distDirLayout = defaultDistDirLayout projectRoot (Just absoluteOutputDir)

    httpTransport <- configureTransport verbosity mempty Nothing

    let httpTransport' =
          httpTransport
            { getHttp = \verbosity' uri _mETag filepath _headers ->
                case uri of
                  URI {uriAuthority = Just (URIAuth {uriRegName = "hackage.haskell.org"})} -> do
                    createFileLink filepath "hackage.haskell.org"
                    return (200, Nothing)
                  _otherwise ->
                    Cabal.die' verbosity' $ "Cannot download " ++ show uri
            }

    (projectConfig, localPackages) <-
      rebuildProjectConfig
        -- more verbose here to list the project files which have affected
        -- the project configuration with no extra options
        (moreVerbose verbosity)
        httpTransport'
        distDirLayout
        mempty

    pPrint projectConfig

    -- Two variants of the install plan are returned: with and without
    -- packages from the store. That is, the "improved" plan where source
    -- packages are replaced by pre-existing installed packages from the
    -- store (when their ids match), and also the original elaborated plan
    -- which uses primarily source packages.
    (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
      rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

    putStrLn $ "Writing detailed plan to " ++ absoluteOutputDir

    writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

    let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]

    for_ ecps $
      \ElaboratedConfiguredPackage
         { elabPkgSourceId,
           elabPkgDescriptionOverride
         } -> do
          let pkgFile = absoluteOutputDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
          for_ elabPkgDescriptionOverride $ \pkgTxt -> do
            Cabal.info verbosity $ "Writing package description for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
            BSL.writeFile pkgFile pkgTxt
