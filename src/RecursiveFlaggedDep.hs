{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module RecursiveFlaggedDep where

import Data.Coerce
import Data.Functor.Foldable
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Types.ComponentDeps
import GHC.Generics

newtype RecursiveFlaggedDep qpn = RecursiveFlaggedDep (FlaggedDep qpn)

type instance Base (RecursiveFlaggedDep qpn) = RecursiveFlaggedDepF qpn

data RecursiveFlaggedDepF qpn f
  = RecursiveFlaggedF (FN qpn) FInfo [f] [f]
  | RecursiveSimpleF (LDep qpn) Component
  | RecursiveStanzaF (SN qpn) [f]
  deriving (Functor, Generic)

instance Recursive (RecursiveFlaggedDep qpn) where
  project (RecursiveFlaggedDep (Flagged fn fInfo trueDeps falseDeps)) =
    RecursiveFlaggedF fn fInfo (coerce trueDeps) (coerce falseDeps)
  project (RecursiveFlaggedDep (Simple lDep comp)) =
    RecursiveSimpleF lDep comp
  project (RecursiveFlaggedDep (Stanza sn trueDeps)) =
    RecursiveStanzaF sn (coerce trueDeps)
