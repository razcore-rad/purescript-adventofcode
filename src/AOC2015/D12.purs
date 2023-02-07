module AOC2015.D12 where

import Prelude

import Data.Argonaut (Json, caseJson, decodeJson, jsonParser, printJsonDecodeError, stringify)
import Data.Array (elem)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (class Foldable, sum)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Foreign.Object as FO
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)


toZero :: ∀ a. a -> Number
toZero = const 0.0


toSum :: ∀ f. Functor f => Foldable f => (Json -> Number) -> f Json -> Number
toSum toNumber x = sum $ toNumber <$> x

computeNumber :: Json -> Number
computeNumber = caseJson toZero toZero identity toZero toSum' toSum'
    where
        toSum' :: ∀ f. Functor f => Foldable f => f Json -> Number
        toSum' x = toSum computeNumber x

computeNumberRed :: Json -> Number
computeNumberRed = caseJson toZero toZero identity toZero toSumArray toSumObject
    where
        toSumArray xs = toSum computeNumberRed xs
        isRedObject obj = elem "\"red\"" $ stringify <$> FO.values obj
        toSumObject obj
            | isRedObject obj = 0.0
            | otherwise = toSum computeNumberRed obj

part :: (Json -> Number) -> String -> Either String Number
part compute str = compute <$> (jsonParser str >>= lmap printJsonDecodeError <<< decodeJson)

part1 :: String -> Either String Number
part1 = part computeNumber

part2 :: String -> Either String Number
part2 = part computeNumberRed


main :: Effect Unit
main = launchAff_ do
    input <- readTextFile UTF8 "./data/aoc2015-d12.json"
    logShow $ part1 input
    logShow $ part2 input
    pure unit
