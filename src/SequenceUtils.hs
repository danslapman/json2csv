module SequenceUtils where

import Data.Foldable (find, foldl', toList)
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import qualified Data.Maybe as Mb (mapMaybe)
import Data.Sequence
import Prelude hiding (concat, elem, foldl, foldl', foldl1, mapM, null, (++))

maybeNes :: Seq a -> Maybe (Seq a)
maybeNes = find (not . null) . Just

uniq :: (Hashable a, Eq a) => Seq a -> Seq a
uniq = snd . foldl' step (HS.empty, empty)
  where
    step (seen, acc) x
      | HS.member x seen = (seen, acc)
      | otherwise        = (HS.insert x seen, acc |> x)

mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe _ Empty = empty
mapMaybe pred dq =
  fromList . Mb.mapMaybe pred . toList $ dq