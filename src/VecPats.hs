{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VecPats (pattern (:::), pattern V_) where

import Data.Maybe
import Data.Vector

uncons :: Vector a -> Maybe (a, Vector a)
uncons v = if null v
  then Nothing
  else Just (unsafeHead v, unsafeTail v)

pattern x ::: xs <- (uncons -> Just (x, xs))
pattern V_ <- (uncons -> Nothing)