module AOC2015.D14 where

import Prelude

import Data.Array (catMaybes, length, range, replicate, zip)
import Data.Either (fromRight)
import Data.Foldable (foldl, maximum)
import Data.Maybe (Maybe)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Parsing (Parser, runParser)
import Parsing.Combinators.Array (many)
import Parsing.Language (emptyDef)
import Parsing.String (anyTill, string)
import Parsing.String.Basic (skipSpaces)
import Parsing.Token (TokenParser, makeTokenParser)


type Input =
  { flyTime :: Int
  , restTime :: Int
  , speed :: Int
  }

input :: String
input = """
Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
Cupid can fly 8 km/s for 17 seconds, but then must rest for 114 seconds.
Prancer can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Donner can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 11 km/s for 12 seconds, but then must rest for 125 seconds.
Comet can fly 21 km/s for 6 seconds, but then must rest for 121 seconds.
Blitzen can fly 18 km/s for 3 seconds, but then must rest for 50 seconds.
Vixen can fly 20 km/s for 4 seconds, but then must rest for 75 seconds.
Dancer can fly 7 km/s for 20 seconds, but then must rest for 119 seconds."""

raceTime :: Int
raceTime = 2503

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

lineParser :: Parser String (Array Input)
lineParser = many do
  _ /\ speed <- skipSpaces *> intParser "km/s"
  _ /\ flyTime <- intParser "seconds"
  _ /\ restTime <- intParser "seconds"
  pure $ {speed, flyTime, restTime}
  where
    intParser end = anyTill $ tokenParser.decimal <* skipSpaces <* string end

input' :: Array Input
input' = fromRight [] $ runParser input lineParser

getDistance :: Int -> Input -> Int
getDistance t r = r.speed * (r.flyTime * wholes + lastFlyTime)
  where
    flyRestTime = r.flyTime + r.restTime
    wholes = t / flyRestTime
    lastFlyTime = min r.flyTime $ t `mod` flyRestTime


part1 :: Maybe Int
part1 = maximum $ map (getDistance raceTime) input'

part2 :: Maybe Int
part2 = maximum $ foldl sumScore (replicate (length input') 0) $ catMaybes do
  t <- range 1 raceTime
  let ds = getDistance t <$> input'
  pure do
    maxDist <- maximum ds
    pure $ (\d -> if maxDist == d then 1 else 0) <$> ds
  where sumScore xs ys = map (uncurry (+)) $ zip xs ys
