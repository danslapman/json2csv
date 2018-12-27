{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DequePatterns where

import Data.Maybe
import Deque

pattern d :|| ds <- (uncons -> Just(d, ds))
pattern D_ = Deque [] []