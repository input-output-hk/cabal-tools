{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Foldable (for_)
import Data.Map
import Distribution.Client.DistDirLayout (DistDirLayout (distProjectCacheFile))
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState)
import Distribution.Client.ProjectConfig (SolverSettings)
import Distribution.Client.SolverInstallPlan (SolverInstallPlan)
import Distribution.Client.SolverInstallPlan qualified as SIP
import Distribution.Client.Types (PackageSpecifier, UnresolvedSourcePackage)
import Distribution.Simple (Compiler, PackageName)
import Distribution.Simple.Program (ConfiguredProgram)
import Distribution.Solver.Types.OptionalStanza (OptionalStanza)
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb)
import Distribution.Solver.Types.SourcePackage (SourcePackage (srcpkgPackageId))
import Distribution.System (Platform)
import Opts (parseOpts)
import Text.Pretty.Simple (CheckColorTty (NoCheckColorTty), OutputOptions (outputOptionsCompact, outputOptionsCompactParens), defaultOutputOptionsDarkBg, pPrintOpt)
import WithCacheFile (withCacheFile)

type Key = (SolverSettings, [PackageSpecifier UnresolvedSourcePackage], Map PackageName (Map OptionalStanza Bool), Compiler, Platform, [ConfiguredProgram])

type Value = (SolverInstallPlan, PkgConfigDb, TotalIndexState, ActiveRepos)

pPrint' :: Show a => a -> IO ()
pPrint' = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg {outputOptionsCompact = True, outputOptionsCompactParens = True}

main :: IO ()
main = do
  (_prjRoot, distDirLayout) <- parseOpts

  withCacheFile @Key @Value (distProjectCacheFile distDirLayout "solver-plan") $ \case
    Left err -> print err
    Right (_monitorStateFileSet, k, Left err) -> do
      print k
      print err
    Right (_monitorStateFileSet, k, Right v) -> do
      let (solverSettings, localPackages, localPackagesEnabledStanzas, compiler, platform, configuredPrograms) = k
      let (solverPlan, _pkgConfigDB, totalIndexState, activeRepos) = v

      putStrLn "-------------------- solverSettings --------------------"
      pPrint' solverSettings

      putStrLn "-------------------- localPackages --------------------"
      for_ localPackages $ pPrint' . fmap srcpkgPackageId

      putStrLn "-------------------- localPackagesEnabledStanzas --------------------"
      pPrint' localPackagesEnabledStanzas

      putStrLn "-------------------- compiler --------------------"
      pPrint' compiler

      putStrLn "-------------------- platform --------------------"
      pPrint' platform

      putStrLn "-------------------- configuredPrograms --------------------"
      pPrint' configuredPrograms

      putStrLn "-------------------- solverPlan --------------------"
      for_ (SIP.toList solverPlan) pPrint'

      putStrLn "-------------------- totalIndexState --------------------"
      pPrint' totalIndexState

      putStrLn "-------------------- activeRepos --------------------"
      pPrint' activeRepos
