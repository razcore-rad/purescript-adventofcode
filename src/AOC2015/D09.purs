module AOC2015.D09 where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.Array as A
import Data.Either (either)
import Data.Foldable (maximum, minimum, sum)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Int as I
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray) as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parsing (Parser, runParser)
import Parsing.String (string)
import Parsing.String.Basic (digit, letter, skipSpaces, upper)


input :: String
input = """
Faerun to Norrath = 129
Faerun to Tristram = 58
Faerun to AlphaCentauri = 13
Faerun to Arbre = 24
Faerun to Snowdin = 60
Faerun to Tambi = 71
Faerun to Straylight = 67
Norrath to Tristram = 142
Norrath to AlphaCentauri = 15
Norrath to Arbre = 135
Norrath to Snowdin = 75
Norrath to Tambi = 82
Norrath to Straylight = 54
Tristram to AlphaCentauri = 118
Tristram to Arbre = 122
Tristram to Snowdin = 103
Tristram to Tambi = 49
Tristram to Straylight = 97
AlphaCentauri to Arbre = 116
AlphaCentauri to Snowdin = 12
AlphaCentauri to Tambi = 18
AlphaCentauri to Straylight = 91
Arbre to Snowdin = 129
Arbre to Tambi = 53
Arbre to Straylight = 40
Snowdin to Tambi = 15
Snowdin to Straylight = 99
Tambi to Straylight = 70"""

city :: Parser String String
city = do
  c <- upper
  rest <- A.many letter
  pure <<< S.fromCharArray $ [c] <> rest

link :: Parser String String
link = skipSpaces *> string "to" <* skipSpaces

assign :: Parser String String
assign = skipSpaces *> string "=" <* skipSpaces

key :: Parser String (Tuple String String)
key = do
  city1 <- city
  city2 <- link *> city
  pure $ city1 /\ city2

value :: Parser String Int
value = fromMaybe 0 <<< I.fromString <<< S.fromCharArray <$> A.many digit

line :: Parser String (Tuple (Tuple String String) Int)
line = do
  k <- skipSpaces *> key
  v <- assign *> value
  pure $ Tuple k v

lines :: Parser String (Array (Tuple (Tuple String String) Int))
lines = A.many line

input' :: HashMap (Tuple String String) Int
input' = either (const HM.empty) HM.fromArray $ runParser input lines

distance :: String -> String -> Maybe Int
distance k1 k2 = HM.lookup (Tuple k1 k2) input' <|> HM.lookup (Tuple k2 k1) input'

length :: Array String -> Maybe Int
length tour = do
  ts1 <- A.init tour
  ts2 <- A.tail tour
  sum $ map (uncurry distance) $ A.zip ts1 ts2

permutations :: Int -> Array (Array Int)
permutations n | n <= 0 = []
permutations 1 = [[1]]
permutations n = do
  p <- permutations (n - 1)
  i <- A.range 0 (n - 1)
  maybe [] pure (A.insertAt i n p)

cities :: Array String
cities = A.nub <<< A.concat $ HM.toArrayBy (\(Tuple k1 k2) _ -> [k1, k2]) input'

tours :: Array (Array String)
tours = do
  indices <- permutations $ A.length cities
  pure $ A.catMaybes $ map (\i -> cities !! (i - 1)) indices

distances :: Array Int
distances = A.catMaybes $ length <$> tours


part1 :: Effect Unit
part1 = logShow $ minimum distances

part2 :: Effect Unit
part2 = logShow $ maximum distances
