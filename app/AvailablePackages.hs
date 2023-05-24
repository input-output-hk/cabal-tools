{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Foldable (for_)
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types.SourcePackageDb
import Distribution.Pretty (prettyShow)
import Distribution.Solver.Types.PackageIndex
import Distribution.Verbosity
import Opts (parseOpts)
import PrettyPrintSimple (pPrint)
import System.Environment (getArgs)

main :: IO ()
main = do
  let verbosity = verbose
  (_prjRoot, distDirLayout) <- parseOpts

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, _localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      mempty

  let ProjectConfig {projectConfigShared} = projectConfig

  let solverSettings = resolveSolverSettings projectConfig

  (sourcePkgDb, _totalIndexState, _activeRepos) <- projectConfigWithSolverRepoContext
    verbosity
    projectConfigShared
    (projectConfigBuildOnly projectConfig)
    $ \repoctx ->
      getSourcePackagesAtIndexState
        verbosity
        repoctx
        (solverSettingIndexState solverSettings)
        (solverSettingActiveRepos solverSettings)

  args <- getArgs

  let SourcePackageDb {packageIndex} = sourcePkgDb
  case args of
    [] -> do
      for_
        (allPackages packageIndex)
        pPrint
    s : _ -> do
      for_ (searchByNameSubstring packageIndex s) $ \(pkgName, pkg) -> do
        putStrLn $ prettyShow pkgName
        pPrint pkg

deriving instance Show SourcePackageDb
