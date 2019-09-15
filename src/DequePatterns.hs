{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DequePatterns where

import Data.Bool
import Data.Maybe
import Deque.Strict

pattern d :|| ds <- (uncons -> Just(d, ds))
pattern D_ <- (null -> True)