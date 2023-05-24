{-# LANGUAGE OverloadedStrings #-}

module SolverIndex where

import ComponentFlaggedDep
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.String
import Distribution.Pretty qualified as Cabal
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Log
import Distribution.Solver.Modular.Package
import FlaggedDep
import Prettyprinter

prettySolverFailure :: SolverFailure -> String
prettySolverFailure BackjumpLimitReached = "BackjumpLimitReached!"
prettySolverFailure (ExhaustiveSearch cs cm) = "ExhaustiveSearch! conflict set: " ++ show cs ++ " conflict map: " ++ show cm

printSolverIndex :: Index -> IO ()
printSolverIndex idx = do
  putStrLn "------------------------"
  putStrLn "----- solver index -----"
  putStrLn "------------------------"
  putStrLn ""

  for_ (Map.toList idx) $ \(pn, mipi) ->
    for_ (Map.toList mipi) $ \(i, PInfo deps mec flagInfo _mfail) -> do
      print $
        vsep $
          [ pretty pn
              <> "-"
              <> fromString (showI i),
            "exposed components:"
              <+> hsep (punctuate comma [prettyExposedComponentInfo ec ci | (ec, ci) <- Map.toList mec])
          ]
            ++ [ "flags:" <+> hsep (punctuate comma [pretty fn | (fn, _fi) <- Map.toList flagInfo])
               ]
            ++ [ "dependencies:",
                 indent 2 $
                   vsep
                     [ vsep
                         [ fromString (Cabal.prettyShow c) <+> "depends on",
                           indent 2 $ prettyComponentFlaggedDeps dep,
                           line
                         ]
                       | (c, dep) <- Map.toList $ toComponents deps
                     ]
               ]
