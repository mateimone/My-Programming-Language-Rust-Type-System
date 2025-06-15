module FiniteMap where

import Control.Exception
import Data.Typeable

data NotFoundException = NotFoundException deriving (Show, Typeable)

-- instance Exception NotFoundException

data FiniteMap k v
  = Empty
  | Node k v (FiniteMap k v) (FiniteMap k v)
  deriving (Show, Eq)

empty :: FiniteMap k v
empty = Empty

bind :: (Ord k) => k -> v -> FiniteMap k v -> FiniteMap k v
bind key val Empty = Node key val Empty Empty
bind key val (Node nk nv m1 m2)
  | key == nk = Node key val m1 m2
  | key < nk = Node nk nv (bind key val m1) m2
  | key > nk = Node nk nv m1 (bind key val m2)

lookupFM :: (Ord k) => k -> FiniteMap k v -> Maybe v
lookupFM key Empty = Nothing
lookupFM key (Node k v m1 m2)
  | key == k = return v
  | key < k = lookupFM key m1
  | key > k = lookupFM key m2
