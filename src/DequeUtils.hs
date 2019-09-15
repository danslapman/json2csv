module DequeUtils where

import Data.Foldable (find, toList)
import qualified Data.List as L
import Data.Maybe (Maybe(Just))
import qualified Data.Maybe as Mb (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V 
import Deque.Strict
import DequePatterns
import GHC.Exts (fromList)
import Prelude hiding ((++), concat, elem, foldl, foldl', foldl1, null, mapM)

empty :: Deque a
empty = fromConsAndSnocLists [] []

maybeNeq :: Deque a -> Maybe (Deque a)
maybeNeq = find (not . null) . Just

uniq :: Eq a => Deque a -> Deque a
uniq = fromList . L.nub . toList

mapMaybe :: (a -> Maybe b) -> Deque a -> Deque b
mapMaybe _ D_ = empty
mapMaybe pred dq =
  fromList . Mb.mapMaybe pred . toList $ dq

fromVector :: Vector a -> Deque a
fromVector = fromList . V.toList