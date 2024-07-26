module Distribution.License.Json where

import Unjson

import Distribution.License qualified as License
import Distribution.SPDX qualified as SPDX

-- | FIXME
deriving via UnjsonShowRead (Either SPDX.License License.License) instance Unjson (Either SPDX.License License.License)

deriving via UnjsonPrettyParsec SPDX.License instance Unjson SPDX.License
