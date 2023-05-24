{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ComponentFlaggedDep where

import Data.Functor.Foldable (cata)
import Data.Map.Monoidal.Strict (getMonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Semialign qualified as Semialign
import Data.String (fromString)
import Data.These
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Types.ComponentDeps
import Distribution.Solver.Types.OptionalStanza
import FlaggedDep
import Prettyprinter

type ComponentFlaggedDeps qpn = [ComponentFlaggedDep qpn]

data ComponentFlaggedDep qpn
  = ComponentSimple
      (LDep qpn)
  | ComponentFlagged
      (FN qpn)
      FInfo
      [ComponentFlaggedDep qpn]
      -- ^ trueDeps
      [ComponentFlaggedDep qpn]
      -- ^ falseDeps
  | ComponentStanza
      (SN qpn)
      [ComponentFlaggedDep qpn]
  deriving (Show)

toComponents :: FlaggedDeps qpn -> Map Component (ComponentFlaggedDeps qpn)
toComponents = getMonoidalMap . foldMap (cata go)
  where
    go (FlaggedF fn fInfo trueDeps falseDeps) =
      Semialign.alignWith
        (\theseDeps -> [uncurry (ComponentFlagged fn fInfo) $ fromThese [] [] theseDeps])
        (mconcat trueDeps)
        (mconcat falseDeps)
    go (SimpleF lDep comp) =
      MonoidalMap.singleton comp [ComponentSimple lDep]
    go (StanzaF sn trueDeps) =
      (\deps -> [ComponentStanza sn deps]) <$> mconcat trueDeps

instance Pretty qpn => Pretty (ComponentFlaggedDep qpn) where
  pretty (ComponentSimple (LDep _dependencyReason dep)) =
    pretty dep
  pretty (ComponentFlagged (FN pn flag) _finfo trueDeps falseDeps)
    | null trueDeps && null falseDeps =
        mempty
    | null falseDeps =
        "when"
          <+> pretty pn
            <> ":"
            <> pretty flag
            <> line
            <> indent 2 (vsep (map pretty trueDeps))
    | null trueDeps =
        "unless"
          <+> pretty pn
            <> ":"
            <> pretty flag
            <> line
            <> indent 2 (vsep (map pretty falseDeps))
    | otherwise =
        "if"
          <+> pretty pn
            <> ":"
            <> pretty flag
            <> line
            <> indent 2 ("then" <+> align (vsep (map pretty trueDeps)))
            <> line
            <> indent 2 ("else" <+> align (vsep (map pretty falseDeps)))
  pretty (ComponentStanza (SN _pn stanza) trueDeps)
    | null trueDeps =
        mempty
    | otherwise =
        "when"
          <+> fromString (showStanza stanza)
          <+> "is enabled"
            <> line
            <> indent 2 (vsep (map pretty trueDeps))

prettyComponentFlaggedDeps :: Pretty qpn => ComponentFlaggedDeps qpn -> Doc ann
prettyComponentFlaggedDeps deps =
  align (vsep [pretty dep | dep <- deps, not (isNull dep)])
  where
    isNull (ComponentFlagged _fn _fInfo trueDeps falseDeps) | null trueDeps && null falseDeps = True
    isNull (ComponentStanza _sn trueDeps) | null trueDeps = True
    isNull _otherwise = False
