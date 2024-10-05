module SequenceUtils where

import Data.Foldable (find, toList)
import qualified Data.List as L
import qualified Data.Maybe as Mb (mapMaybe)
import Data.Sequence
import Prelude hiding (concat, elem, foldl, foldl', foldl1, mapM, null, (++))

maybeNes :: Seq a -> Maybe (Seq a)
maybeNes = find (not . null) . Just

uniq :: Eq a => Seq a -> Seq a
uniq = fromList . L.nub . toList

mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe _ Empty = empty
mapMaybe pred dq =
  fromList . Mb.mapMaybe pred . toList $ dq