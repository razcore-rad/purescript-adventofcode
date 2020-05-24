module AOC2019.D02 (part1, part2) where

import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, length, updateAt, (!!), (..))
import Data.Foldable (or)
import Data.Int (fromString)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

input :: String
input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0"

input' :: Array Int
input' = catMaybes $ fromString <$> split (Pattern ",") input


runProgram ∷ Array Int → Maybe (Array Int)
runProgram = runProgram' 0
  where ops ∷ ∀ a. Semiring a ⇒ Map Int (a → a → a)
        ops = fromFoldable [Tuple 1 (+), Tuple 2 (*)]

        runProgram' i as
          | i < length as = runProgram' (i + 4) as'
          where maybe_ai = as!!i
                as' | or $ sequence ((==) <$> Just <$> [1, 2]) maybe_ai =
                      fromMaybe [] $ do
                        ai ← maybe_ai
                        ix ← as!!(i + 1)
                        iy ← as!!(i + 2)
                        iz ← as!!(i + 3)
                        result ← lookup ai ops <*> as!!ix <*> as!!iy
                        updateAt iz result as
                    | (Just 99) == as!!i = as
                    | otherwise          = []
          | otherwise = Just as

part1 :: Maybe (Array Int)
part1 = do
  p' <- updateAt 1 12 input'
  p  <- updateAt 2  2 p'
  runProgram p


part2 :: Array Int
part2 = do
  noun <- 0 .. 99
  verb <- 0 .. 99
  let result = fromMaybe [] do
                p' <- updateAt 1 noun input'
                p  <- updateAt 2 verb p'
                runProgram p
  guard $ target == result !! 0
  pure $ 100 * noun + verb
  where target = Just 19_690_720

