module Main where

import Distribution.Client.DistDirLayout (defaultDistDirLayout)
import Distribution.Client.HttpUtils (HttpTransport (..), configureTransport)
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

  let patchedHttpTransport =
        httpTransport
          { getHttp = \verbosity' uri mETag fp headers -> do
              putStrLn $ "download uri " ++ show uri ++ " to " ++ show fp
              getHttp httpTransport verbosity' uri mETag fp headers
          }

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      patchedHttpTransport
      distDirLayout
      mempty

  putStrLn "done"

  pPrint projectConfig
  pPrint localPackages
