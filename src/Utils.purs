module Utils where

import Prelude

import Control.Plus (empty)
import Data.Array (insertAt, range)
import Data.Maybe (maybe)


permutations :: Int -> Array (Array Int)
permutations n | n <= 0 = []
permutations 1 = [[0]]
permutations n = do
  p <- permutations n'
  i <- range 0 n'
  maybe empty pure $ insertAt i n' p
  where n' = n - 1
