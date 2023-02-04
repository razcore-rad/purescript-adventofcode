module AOC2015.D10 where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold)
import Data.Int (decimal, toStringAs)
import Data.String.CodeUnits as S

input :: String
input = "3113322113"

lookSay :: String -> String
lookSay s = fold $ map (\as -> (toStringAs decimal $ NEA.length as) <> S.fromCharArray [NEA.head as]) $ A.group $ S.toCharArray s

runLookSay :: Int -> String -> String
runLookSay n s | n <= 0 = s
runLookSay n s = runLookSay (n - 1) $ lookSay s

part1 :: String -> Int
part1 = S.length <<< runLookSay 40

part2 :: String -> Int
part2 = S.length <<< runLookSay 50
