{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FlaggedDep where

import Data.Coerce
import Data.Functor.Foldable
import Data.String
import Distribution.Compat.Lens
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty qualified as Cabal
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Types.ComponentDeps
import Distribution.Solver.Types.OptionalStanza
import Distribution.Types.ComponentName
import Distribution.Types.VersionRange
import GHC.Generics
import Prettyprinter

type instance Base (FlaggedDep qpn) = FlaggedDepF qpn

data FlaggedDepF qpn f
  = FlaggedF (FN qpn) FInfo [f] [f]
  | SimpleF (LDep qpn) Component
  | StanzaF (SN qpn) [f]
  deriving (Functor, Generic)

instance Recursive (FlaggedDep qpn) where
  project (Flagged fn fInfo trueDeps falseDeps) =
    FlaggedF fn fInfo (coerce trueDeps) (coerce falseDeps)
  project (Simple lDep comp) =
    SimpleF lDep comp
  project (Stanza sn trueDeps) =
    StanzaF sn (coerce trueDeps)

flaggedDepQPN :: Traversal (FlaggedDep a) (FlaggedDep b) a b
flaggedDepQPN f (Simple lDep component) =
  Simple <$> lDepQPN f lDep <*> pure component
flaggedDepQPN f (Flagged fn fInfo trueDeps falseDeps) =
  Flagged <$> fnQPN f fn <*> pure fInfo <*> (traverse . flaggedDepQPN) f trueDeps <*> (traverse . flaggedDepQPN) f falseDeps
flaggedDepQPN f (Stanza sn trueDeps) =
  Stanza <$> snQPN f sn <*> (traverse . flaggedDepQPN) f trueDeps

flaggedDepComponent :: Traversal' (FlaggedDep qpn) Component
flaggedDepComponent f (Simple lDep component) =
  Simple lDep <$> f component
flaggedDepComponent f (Flagged fn fInfo trueDeps falseDeps) =
  Flagged fn fInfo <$> (traverse . flaggedDepComponent) f trueDeps <*> (traverse . flaggedDepComponent) f falseDeps
flaggedDepComponent f (Stanza sn trueDeps) =
  Stanza sn <$> (traverse . flaggedDepComponent) f trueDeps

lDepQPN :: Traversal (LDep a) (LDep b) a b
lDepQPN f (LDep depr dep) = LDep <$> dependecyReasonQPN f depr <*> depQPN f dep

depQPN :: Traversal (Dep a) (Dep b) a b
depQPN f (Dep (PkgComponent qpn ec) ci) = Dep <$> (PkgComponent <$> f qpn <*> pure ec) <*> pure ci
depQPN _ (Ext e) = pure (Ext e)
depQPN _ (Lang l) = pure (Lang l)
depQPN _ (Pkg pn pv) = pure (Pkg pn pv)

dependecyReasonQPN :: Traversal (DependencyReason a) (DependencyReason b) a b
dependecyReasonQPN f (DependencyReason qpn m s) = DependencyReason <$> f qpn <*> pure m <*> pure s

fnQPN :: Traversal (FN a) (FN b) a b
fnQPN f (FN qpn flag) = FN <$> f qpn <*> pure flag

snQPN :: Traversal (SN a) (SN b) a b
snQPN f (SN qpn stanza) = SN <$> f qpn <*> pure stanza

--
--
--

deriving instance Show qpn => Show (FlaggedDep qpn)

deriving instance Show qpn => Show (Dep qpn)

deriving instance Show qpn => Show (LDep qpn)

--
-- Pretty printing
--

instance Pretty qpn => Pretty (FlaggedDep qpn) where
  pretty (Simple (LDep _dependencyReason dep) component) =
    fromString (Cabal.prettyShow component) <+> "depends on" <+> pretty dep
  pretty (Flagged (FN pn flag) _finfo trueDeps falseDeps)
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
  pretty (Stanza (SN _pn stanza) trueDeps)
    | null trueDeps =
        mempty
    | otherwise =
        "when"
          <+> fromString (showStanza stanza)
          <+> "is enabled"
            <> line
            <> indent 2 (vsep (map pretty trueDeps))

instance Pretty qpn => Pretty (Dep qpn) where
  pretty (Dep pkgC ci) =
    pretty pkgC <+> pretty ci
  pretty (Ext extension) =
    "extension" <+> fromString (show extension)
  pretty (Lang language) =
    "language" <+> fromString (show language)
  pretty (Pkg pkgConfigName pkgConfigVR) =
    fromString (Cabal.prettyShow pkgConfigName) <+> fromString (Cabal.prettyShow pkgConfigVR)

instance Pretty CI where
  pretty (Fixed i) = "fixed" <+> fromString (showI i)
  pretty (Constrained vr) | isAnyVersion vr = ""
  pretty (Constrained vr) = fromString (Cabal.prettyShow vr)

instance Pretty ExposedComponent where
  pretty (ExposedLib libName) = pretty $ PD.CLibName libName
  pretty (ExposedExe exeName) = pretty $ PD.CExeName exeName

instance Pretty qpn => Pretty (PkgComponent qpn) where
  pretty (PkgComponent pn ec) = pretty pn <> ":" <> pretty ec

instance Pretty ComponentName where
  pretty = fromString . Cabal.prettyShow

instance Pretty Flag where
  pretty = fromString . Cabal.prettyShow

instance Pretty PN where
  pretty = fromString . Cabal.prettyShow

prettyExposedComponentInfo :: ExposedComponent -> ComponentInfo -> Doc ann
prettyExposedComponentInfo ec ci = pretty ec <+> "(" <> prettyCompInfo ci <> ")"
  where
    prettyCompInfo (ComponentInfo visible buildable) = hsep [prettyIV visible, prettyIB buildable]

    prettyIV (IsVisible True) = "visible"
    prettyIV (IsVisible False) = mempty

    prettyIB (IsBuildable True) = "buildable"
    prettyIB (IsBuildable False) = mempty
