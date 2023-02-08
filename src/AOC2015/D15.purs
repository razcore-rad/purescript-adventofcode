module AOC2015.D15 where

import Prelude

import Control.Alternative (guard)
import Data.Array (all, filter, length, range, replicate, transpose, zip)
import Data.Either (fromRight)
import Data.Foldable (maximum, product, sum)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.String (anyTill)
import Parsing.Token (TokenParser, makeTokenParser)
import Utils (cartesianProduct)

type Ingredients = 
  { capacity :: Int
  , durability :: Int
  , flavor :: Int
  , texture :: Int
  , calories :: Int
  }

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

lineParser :: Parser String Ingredients
lineParser = do
  capacity <- ingredientParser
  durability <- ingredientParser
  flavor <- ingredientParser
  texture <-ingredientParser
  calories <- ingredientParser
  pure {capacity, durability, flavor, texture, calories}
  where ingredientParser = map snd $ anyTill $ tokenParser.integer

testInput :: String
testInput = """
Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"""

testInput' :: Array Ingredients
testInput' = fromRight [] $ runParser testInput $ many lineParser

input :: String
input = """
Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8"""

input' :: Array Ingredients
input' = fromRight [] $ runParser input $ many lineParser

totalTeaspoons :: Int
totalTeaspoons = 100

teaspoonDistributions :: Int -> Array (Array Int)
teaspoonDistributions n = do
  r <- cartesianProduct $ replicate (n - 1) $ range 1 $ totalTeaspoons - n + 1
  let last = totalTeaspoons - sum r
  guard $ last > 0
  pure $ r <> [last]

scores :: Array (Array Int) -> Array Ingredients -> Array (Array Int)
scores ts xs = map (map sum) transpose <$> do
  teaspoons <- ts
  pure $ (\(n /\ x) -> [n * x.capacity, n * x.durability, n * x.flavor, n * x.texture]) <$> zip teaspoons xs


part1 :: Array Ingredients  -> Maybe Int
part1 xs = maximum $ map product $ filter (all (_ > 0)) $ scores teaspoonss xs
  where teaspoonss = teaspoonDistributions $ length xs

part2 :: Array Ingredients -> Maybe Int
part2 xs = maximum $ map (product <<< snd) $ filter ((500 == _) <<< fst && (all (_ > 0) <<< snd)) $ zip calories $ scores teaspoonss xs
  where
    teaspoonss = teaspoonDistributions $ length xs
    calories = sum <$> do
      teaspoons <- teaspoonss
      pure $ (\(n /\ x) -> n * x.calories) <$> zip teaspoons xs
