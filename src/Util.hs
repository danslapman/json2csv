module Util where

import Control.Monad ((>=>))
import Data.Foldable (find)
import Data.List (union)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just))
import Data.Vector hiding (find)
import Prelude hiding ((++), concat, elem, foldl, foldl1, null, mapM)

maybeNev :: Vector a -> Maybe (Vector a)
maybeNev = find (not . null) . Just

vconcat :: Vector (Vector a) -> Vector a
vconcat = foldl (++) empty

(|=>) :: Monad f => (a -> f (Vector b)) -> (b -> f (Vector c)) -> a -> f (Vector c)
(|=>) fx fy v = fmap vconcat (fx >=> (mapM fy) $ v)

vunion :: Eq a => Vector a -> Vector a -> Vector a
vunion =
  let setadd vec a | not $ elem a vec = vec `snoc` a
      setadd vec _ = vec
  in foldl setadd 

concatUnion :: Eq a => Vector (Vector a) -> Vector a
concatUnion = foldl1 vunion

hmToVector :: HM.HashMap a b -> Vector (a,b)
hmToVector = fromList . HM.toList