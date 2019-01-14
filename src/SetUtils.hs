module SetUtils where

import Data.Foldable (find)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(Just), mapMaybe)
import Data.Set.Monad
import qualified Data.Vector as V
import Prelude hiding (null)

maybeNes :: Ord a => Set a -> Maybe (Set a)
maybeNes = find (not . null) . Just

fromHashMap :: (Ord a, Ord b) => HM.HashMap a b -> Set (a,b)
fromHashMap = fromList . HM.toList