module Utils where

import Prelude

import Control.Plus (empty)
import Data.Array (concatMap, insertAt, range, snoc)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (maybe)


permutations :: Int -> Array (Array Int)
permutations n | n <= 0 = []
permutations 1 = [[0]]
permutations n = do
  p <- permutations n'
  i <- range 0 n'
  maybe empty pure $ insertAt i n' p
  where n' = n - 1

cartesianProduct :: ∀ f a. Foldable f => f (Array a) -> Array (Array a)
cartesianProduct = foldl (\acc pool -> concatMap (\a -> (a `snoc` _) <$> pool) acc) [[]]
