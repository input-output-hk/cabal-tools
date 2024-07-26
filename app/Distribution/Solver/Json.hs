module Distribution.Solver.Json where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable

import Distribution.Parsec
import Distribution.Pretty (prettyShow)

import Distribution.Solver.Types.ComponentDeps
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.ProjectConfigPath

import Data.Functor.Invariant
import Unjson

import Distribution.Json ()
import Distribution.Types.Json ()

deriving via (NonEmpty FilePath) instance Unjson ProjectConfigPath

deriving via UnjsonEnum OptionalStanza instance Unjson OptionalStanza

instance (Monoid a, Typeable a, Unjson a) => Unjson (ComponentDeps a) where
    unjsonDef = invmap fromList toList unjsonDef

instance Unjson Component where
    unjsonDef = unjsonInvmapR parse' prettyShow (unjsonDef @String)
      where
        parse' :: String -> Result Component
        parse' =
            either fail return
                . explicitEitherParsec
                    ( componentNameToComponent
                        <$> parsec
                        <|> error "fixme, missing setup"
                    )

-- instance Unjson ComponentRequestedSpec where
--     unjsonDef =
--         disjointUnionOf
--             "ComponentRequestedSpec"
--             [ gConst "ComponentRequestedSpec" "ComponentRequestedSpec" ComponentRequestedSpec (\case ~(ComponentRequestedSpec test bench) -> _)
--             , gConst "OneComponentRequestedSpec" "OneComponentRequestedSpec" OneComponentRequestedSpec (\case ~(OneComponentRequestedSpec name) -> name)
--             ]

instance Unjson OptionalStanzaSet where
    unjsonDef = invmap optStanzaSetFromList optStanzaSetToList unjsonDef

instance Unjson (OptionalStanzaMap (Maybe Bool)) where
    unjsonDef = invmap optStanzaTabulate optStanzaIndex unjsonTotalMap
