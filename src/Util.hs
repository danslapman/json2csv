module Util where

import Control.Monad ((>=>))
import Data.Foldable (find)

maybeNel :: [a] -> Maybe [a]
maybeNel = find (not . null) . Just

(|=>) :: Monad f => (a -> f [b]) -> (b -> f [c]) -> a -> f [c]
(|=>) fx fy v = fmap concat (fx >=> (mapM fy) $ v)