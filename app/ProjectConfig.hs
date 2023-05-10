{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.ProjectConfig (ProjectConfig)
import Distribution.Client.ProjectPlanning (ElaboratedInstallPlan, ElaboratedSharedConfig)
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Opts (parseOpts)
import Text.Pretty.Simple (CheckColorTty (NoCheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsDarkBg, pPrintOpt)
import WithCacheFile (withCacheFile)

type Key = (ProjectConfig, [PackageSpecifier UnresolvedSourcePackage], [FilePath])

type Value = (ElaboratedInstallPlan, ElaboratedInstallPlan, ElaboratedSharedConfig, TotalIndexState, ActiveRepos)

pPrint' :: Show a => a -> IO ()
pPrint' = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True, outputOptionsCompactParens = True}

main :: IO ()
main = do
  (_prjRoot, distDirLayout) <- parseOpts

  withCacheFile @Key @Value (distProjectCacheFile distDirLayout "improved-plan") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right _v) -> do
      let (projectConfig, localPackages, _progSearchPath) = k

      pPrint' projectConfig
      pPrint' localPackages
