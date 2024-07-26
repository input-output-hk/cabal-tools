module Distribution.Backpack.Json where

import Distribution.Backpack
import Distribution.Backpack.ModuleShape

import Distribution.Json ()

import Unjson

deriving via UnjsonPrettyParsec OpenModule instance Unjson OpenModule

deriving via UnjsonPrettyParsec OpenUnitId instance Unjson OpenUnitId

deriving via UnjsonObject ModuleShape instance Unjson ModuleShape
