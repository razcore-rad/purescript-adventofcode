module AOC2015.D04 (part1, part2) where

import Prelude

import Data.String.CodeUnits (countPrefix)
import Effect (Effect)
import Node.Crypto.Hash (Algorithm(..), hex)


input :: String
input = "ckczppom"


solveZeros :: Int -> Int -> String -> Effect Int
solveZeros i n s = do
  hash <- hex MD5 $ s <> show i
  if countPrefix ('0' == _) hash == n
    then pure i
    else solveZeros (i + 1) n s


part1 :: Effect Int
part1 = solveZeros 0 5 input


part2 :: Effect Int
part2 = do
  part1Result <- part1
  solveZeros part1Result 6 input

