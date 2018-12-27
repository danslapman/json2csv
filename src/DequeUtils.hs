module DequeUtils where

import Control.Monad ((>=>))
import Data.Foldable (elem, foldl, foldl1, find, toList)
import Data.List (nub)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just))
import Data.Traversable (mapM)
import Data.Vector (Vector)
import qualified Data.Vector as V 
import Deque
import DequePatterns
import Prelude hiding ((++), concat, elem, foldl, foldl1, null, mapM)

null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

empty :: Deque a
empty = Deque [] []

maybeNeq :: Deque a -> Maybe (Deque a)
maybeNeq = find (not . null) . Just

union :: Eq a => Deque a -> Deque a -> Deque a
union =
  let setadd deq a | not $ elem a deq = a `snoc` deq
      setadd deq _ = deq
  in foldl setadd

hmToDeque :: HM.HashMap a b -> Deque (a,b)
hmToDeque = fromList . HM.toList

uniq :: Eq a => Deque a -> Deque a
uniq = fromList . nub . toList

mapMaybe :: (a -> Maybe b) -> Deque a -> Deque b
mapMaybe _ (Deque [] []) = empty
mapMaybe pred (h :|| t) =
  case pred h of
    Just nb -> nb `cons` mapMaybe pred t
    Nothing ->  mapMaybe pred t

fromVector :: Vector a -> Deque a
fromVector = fromList . V.toList