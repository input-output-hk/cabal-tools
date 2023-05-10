{-# LANGUAGE LambdaCase #-}

module Opts where

import Control.Monad (join)
import Distribution.Client.Compat.Prelude (exitFailure)
import Distribution.Client.DistDirLayout (DistDirLayout, ProjectRoot, defaultDistDirLayout)
import Distribution.Client.ProjectConfig (findProjectRoot)
import Options.Applicative

parseOpts :: IO (ProjectRoot, DistDirLayout)
parseOpts = do
  join $ execParser $ info opts mempty
  where
    opts =
      ( \io distDir ->
          io >>= \case
            Left err ->
              print err >> exitFailure
            Right prjRoot ->
              pure (prjRoot, defaultDistDirLayout prjRoot distDir)
      )
        <$> ( findProjectRoot
                <$> optional (strOption $ long "project-root")
                <*> optional (strOption $ long "project-file")
            )
        <*> optional (strOption $ long "builddir")
