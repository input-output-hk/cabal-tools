{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (when)
import Data.Foldable (for_)
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types.SourcePackageDb
import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.PackageIndex
import Distribution.Solver.Types.SourcePackage
import Distribution.Verbosity
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  let verbosity = verbose

  Right prjRoot <- findProjectRoot Nothing Nothing
  let distDirLayout = defaultDistDirLayout prjRoot Nothing

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

  let SourcePackageDb {packageIndex} = sourcePkgDb
  for_ (allPackagesByName packageIndex) $
    traverse $ \sp ->
      when (pkgName (srcpkgPackageId sp) == "flat") $
        pPrint sp

deriving instance Show SourcePackageDb
