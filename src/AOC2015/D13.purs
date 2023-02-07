module AOC2015.D13 where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Either (fromRight)
import Data.Foldable (foldl, maximum, sum)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Int (fromString)
import Data.Maybe (Maybe, maybe)
import Data.String.CodeUnits as S
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Parsing (Parser, fail, runParser)
import Parsing.Combinators (choice)
import Parsing.Combinators.Array (many)
import Parsing.String (anyTill, char, string)
import Parsing.String.Basic (digit, lower, skipSpaces, upper)
import Utils (permutations)

testInput :: String
testInput = """
Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."""

input :: String
input = """
Alice would gain 2 happiness units by sitting next to Bob.
Alice would gain 26 happiness units by sitting next to Carol.
Alice would lose 82 happiness units by sitting next to David.
Alice would lose 75 happiness units by sitting next to Eric.
Alice would gain 42 happiness units by sitting next to Frank.
Alice would gain 38 happiness units by sitting next to George.
Alice would gain 39 happiness units by sitting next to Mallory.
Bob would gain 40 happiness units by sitting next to Alice.
Bob would lose 61 happiness units by sitting next to Carol.
Bob would lose 15 happiness units by sitting next to David.
Bob would gain 63 happiness units by sitting next to Eric.
Bob would gain 41 happiness units by sitting next to Frank.
Bob would gain 30 happiness units by sitting next to George.
Bob would gain 87 happiness units by sitting next to Mallory.
Carol would lose 35 happiness units by sitting next to Alice.
Carol would lose 99 happiness units by sitting next to Bob.
Carol would lose 51 happiness units by sitting next to David.
Carol would gain 95 happiness units by sitting next to Eric.
Carol would gain 90 happiness units by sitting next to Frank.
Carol would lose 16 happiness units by sitting next to George.
Carol would gain 94 happiness units by sitting next to Mallory.
David would gain 36 happiness units by sitting next to Alice.
David would lose 18 happiness units by sitting next to Bob.
David would lose 65 happiness units by sitting next to Carol.
David would lose 18 happiness units by sitting next to Eric.
David would lose 22 happiness units by sitting next to Frank.
David would gain 2 happiness units by sitting next to George.
David would gain 42 happiness units by sitting next to Mallory.
Eric would lose 65 happiness units by sitting next to Alice.
Eric would gain 24 happiness units by sitting next to Bob.
Eric would gain 100 happiness units by sitting next to Carol.
Eric would gain 51 happiness units by sitting next to David.
Eric would gain 21 happiness units by sitting next to Frank.
Eric would gain 55 happiness units by sitting next to George.
Eric would lose 44 happiness units by sitting next to Mallory.
Frank would lose 48 happiness units by sitting next to Alice.
Frank would gain 91 happiness units by sitting next to Bob.
Frank would gain 8 happiness units by sitting next to Carol.
Frank would lose 66 happiness units by sitting next to David.
Frank would gain 97 happiness units by sitting next to Eric.
Frank would lose 9 happiness units by sitting next to George.
Frank would lose 92 happiness units by sitting next to Mallory.
George would lose 44 happiness units by sitting next to Alice.
George would lose 25 happiness units by sitting next to Bob.
George would gain 17 happiness units by sitting next to Carol.
George would gain 92 happiness units by sitting next to David.
George would lose 92 happiness units by sitting next to Eric.
George would gain 18 happiness units by sitting next to Frank.
George would gain 97 happiness units by sitting next to Mallory.
Mallory would gain 92 happiness units by sitting next to Alice.
Mallory would lose 96 happiness units by sitting next to Bob.
Mallory would lose 51 happiness units by sitting next to Carol.
Mallory would lose 81 happiness units by sitting next to David.
Mallory would gain 31 happiness units by sitting next to Eric.
Mallory would lose 73 happiness units by sitting next to Frank.
Mallory would lose 89 happiness units by sitting next to George."""

nameP :: Parser String String
nameP = do
  c <- upper
  cs <- many lower
  pure $ S.fromCharArray $ [c] <> cs


weightP :: Parser String Int
weightP = do
  x <- multiplier <$> (choice $ string <$> ["gain", "lose"])
  n <- fromString <<< S.fromCharArray <$> (skipSpaces *> many digit)
  maybe (fail "not digits") pure $ (x * _) <$> n
  where
    multiplier "lose" = -1
    multiplier "gain" = 1
    multiplier _      = 0

lineP :: Parser String (Tuple (Tuple String String) Int)
lineP = do
  v1 <- skipSpaces *> nameP
  Tuple _ w <- anyTill weightP
  Tuple _ v2 <- anyTill (nameP <* char '.')
  pure $ (v1 /\ v2) /\ w

toMap :: String -> HashMap (Tuple String String) Int
toMap s = HM.fromArray $ fromRight [] $ runParser s $ many lineP

input' :: HashMap (Tuple String String) Int
input' = toMap input

testInput' :: HashMap (Tuple String String) Int
testInput' = toMap testInput

getPeople :: HashMap (Tuple String String) Int -> Array String
getPeople hm = A.nub <<< A.concat $ HM.toArrayBy (\(Tuple p1 p2) _ -> [p1, p2]) hm

getArrangements :: HashMap (Tuple String String) Int -> Array (Array String)
getArrangements hm = do
    arrangement <- initArrangement
    pure $ A.catMaybes $ arrangement <> [join $ arrangement !! 0]
  where
    people = getPeople hm
    initArrangement = do
      indices <- permutations $ A.length people
      pure $ (people !! _) <$> indices

getHappiness :: Array String -> HashMap (Tuple String String) Int -> Maybe Int
getHappiness arrangement hm = sum <<< A.catMaybes <$> do
  as <- A.zip <$> A.init arrangement <*> A.tail arrangement
  pure do
    k@(p1 /\ p2) <- as
    pure $ HM.lookup k hm + HM.lookup (p2 /\ p1) hm

insertSelf :: HashMap (Tuple String String) Int -> HashMap (Tuple String String) Int
insertSelf hm = foldl (\acc x -> HM.insert (x /\ "Self") 0 $ HM.insert ("Self" /\ x) 0 acc) hm $ getPeople hm

getOptimalHappines :: HashMap (Tuple String String) Int -> Maybe Int
getOptimalHappines hm = join $ maximum do
  arrangement <- getArrangements hm
  pure $ getHappiness arrangement hm


main :: Effect Unit
main = do
  logShow $ "Part 1:" <> show (getOptimalHappines input')
  logShow $ "Part 2:" <> show (getOptimalHappines $ insertSelf input')
