module AOC2015.D17 where

import Prelude

import Data.Array (length, uncons, (:))
import Data.Maybe (Maybe(..))


testInput :: Array Int
testInput = [20, 15, 10, 5, 5]

testTotal :: Int
testTotal = 25

input :: Array Int
input = [50, 44, 11, 49, 42, 46, 18, 32, 26, 40, 21, 7, 18, 43, 10, 47, 36, 24, 22, 40]

total :: Int
total = 150

combinations :: Int -> Array Int -> Array (Array Int)
combinations 0 _ = [[]]
combinations vol xs =
  case uncons xs of
    Just { head: c, tail: cs } ->
      let
        without = combinations vol cs
        with = (c : _) <$> combinations (vol - c) cs
      in
        if c <= vol then with <> without else without
    Nothing -> []


part1 :: Int -> Array Int -> Int
part1 vol = length <<< combinations vol
