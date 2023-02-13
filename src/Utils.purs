module Utils where

import Prelude

import Control.Alternative (guard)
import Data.Array (concatMap, insertAt, length, nub, range, replicate, snoc, sort, unsafeIndex)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (maybe)
import Partial.Unsafe (unsafePartial)


permutations :: Int -> Array (Array Int)
permutations n | n <= 0 = []
permutations 1 = [[0]]
permutations n = do
  p <- permutations n'
  i <- range 0 n'
  maybe [] pure $ insertAt i n' p
  where n' = n - 1

cartesianProduct :: ∀ f a. Foldable f => f (Array a) -> Array (Array a)
cartesianProduct = foldl (\acc pool -> concatMap (\as -> snoc as <$> pool) acc) [[]]

-- combinations xs n = do
--   indices <- permutations n
--   guard $ sort indices == indices
--   pure $ (xs !! _) <$> indices

-- combinations :: ∀ a. Array a -> Int -> Array (Array a)
-- combinations xs n = do
--   indices <- permutations (range 0 $ length xs - 1) n
--   guard $ sort indices == indices
--   pure $ unsafePartial $ unsafeIndex xs <$> indices

-- permutations :: ∀ a. Array a -> Int -> Array (Array a)
-- permutations xs n = do
--   indices <- cartesianProduct $ replicate n $ range 0 $ length xs - 1
--   guard $ length (nub indices) == n
--   pure $ unsafePartial $ unsafeIndex xs <$> indices
