module Env where

import qualified Data.Map as Map

import Lang.Abs ( Ident )
import Value (Mutability)

type Env a = Map.Map Ident (a, Mutability)

empty :: Env a
empty = Map.empty

find :: Ident -> Env a -> Maybe (a, Mutability)
find = Map.lookup

bind :: Ident -> (a, Mutability) -> Env a -> Env a
bind = Map.insert
