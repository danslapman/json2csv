module HashSetUtils where

import Data.Foldable (find)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just), mapMaybe)
import Data.Hashable
import Data.HashSet
import qualified Data.Vector as V
import Prelude hiding (null)

maybeNes :: HashSet a -> Maybe (HashSet a)
maybeNes = find (not . null) . Just

fromHashMap :: (Eq a, Hashable a, Eq b, Hashable b) => HM.HashMap a b -> HashSet (a,b)
fromHashMap = fromList . HM.toList