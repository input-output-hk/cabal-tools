module Network.Json where

import Network.URI
import Unjson

instance Unjson URI where
    unjsonDef =
        unjsonInvmapR
            (maybe (fail "cannot parse URI") pure . parseURI)
            show
            (unjsonDef @String)
