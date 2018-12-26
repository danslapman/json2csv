module DequeUtils where

import Control.Monad ((>=>))
import Data.Foldable (elem, foldl, foldl1, find)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just))
import Data.Traversable (mapM)
import Deque
import Prelude hiding ((++), concat, elem, foldl, foldl1, null, mapM)

null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

maybeNeq :: Deque a -> Maybe (Deque a)
maybeNeq = find (not . null) . Just

union :: Eq a => Deque a -> Deque a -> Deque a
union =
  let setadd deq a | not $ elem a deq = a `snoc` deq
      setadd deq _ = deq
  in foldl setadd

hmToDeque :: HM.HashMap a b -> Deque (a,b)
hmToDeque = fromList . HM.toList