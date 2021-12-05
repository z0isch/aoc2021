{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( plus2,
    histogram,
  )
where

import qualified Data.HashMap.Monoidal as MonoidalHashMap
import Data.Semigroup (Sum (Sum, getSum))
import RIO

plus2 :: Int -> Int
plus2 = (+ 2)

histogram :: (Eq k, Hashable k, Foldable f) => f k -> [(k, Integer)]
histogram = MonoidalHashMap.toList . fmap getSum . foldMap (\c -> MonoidalHashMap.singleton c (Sum 1))
