module DequeUtils where

import Data.Foldable (find, toList)
import qualified Data.List as L
import qualified Data.Maybe as Mb (mapMaybe)
import Deque.Strict
import DequePatterns
import GHC.Exts (fromList)
import Prelude hiding (concat, elem, foldl, foldl', foldl1, mapM, null, (++))

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