{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Foldable (for_)
import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types.SourcePackageDb
import Distribution.Pretty (prettyShow)
import Distribution.Solver.Types.PackageIndex
import Distribution.Verbosity
import System.Environment (getArgs)
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

    let ProjectConfig{projectConfigShared} = projectConfig

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

    let SourcePackageDb{packageIndex} = sourcePkgDb
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
