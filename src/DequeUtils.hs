module DequeUtils where

import Control.Monad ((>=>))
import Data.Foldable (foldl', find, toList)
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just))
import qualified Data.Maybe as Mb (mapMaybe)
import Data.Traversable (mapM)
import Data.Vector (Vector)
import qualified Data.Vector as V 
import Deque
import Prelude hiding ((++), concat, elem, foldl, foldl', foldl1, null, mapM)

null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

empty :: Deque a
empty = Deque [] []

elem :: Eq a => a -> Deque a -> Bool
elem el (Deque cl sl) =
  L.elem el cl || L.elem el sl

maybeNeq :: Deque a -> Maybe (Deque a)
maybeNeq = find (not . null) . Just

union :: Eq a => Deque a -> Deque a -> Deque a
union =
  let setadd deq a | not $ elem a deq = a `snoc` deq
      setadd deq _ = deq
  in foldl' setadd

hmToDeque :: HM.HashMap a b -> Deque (a,b)
hmToDeque = fromList . HM.toList

uniq :: Eq a => Deque a -> Deque a
uniq = fromList . L.nub . toList

mapMaybe :: (a -> Maybe b) -> Deque a -> Deque b
mapMaybe _ (Deque [] []) = empty
mapMaybe pred (Deque cl sl) =
  let ncl = Mb.mapMaybe pred cl
      nsl = Mb.mapMaybe pred sl
  in Deque ncl nsl

fromVector :: Vector a -> Deque a
fromVector = fromList . V.toList