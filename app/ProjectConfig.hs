module Main where

import Distribution.Client.DistDirLayout (defaultDistDirLayout)
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig (findProjectRoot)
import Distribution.Client.ProjectPlanning (rebuildProjectConfig)
import Distribution.Verbosity (deafening)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  let verbosity = deafening

  Right prjRoot <- findProjectRoot Nothing Nothing

  let distDirLayout = defaultDistDirLayout prjRoot Nothing

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      mempty

  putStrLn "done"

  pPrint projectConfig
  pPrint localPackages
