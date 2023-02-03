module AOC2015.D11 where

import Prelude

import Control.Alternative (guard)
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Enum (fromEnum, toEnumWithDefaults)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), length) as S
import Data.String.CodeUnits (contains, fromCharArray, toCharArray) as S

input :: String
input = "vzbxkghb"

bottomASCII :: Int
bottomASCII = 97

topASCII :: Int
topASCII = 122

toEnumASCII :: Int -> Char
toEnumASCII x = toEnumWithDefaults 'a' 'z' $ if x < bottomASCII || x > topASCII then bottomASCII else x

toInts :: String -> Array Int
toInts s = fromEnum <$> S.toCharArray s

getNs :: forall a. Int -> Int -> Array a -> Array a
getNs n i = A.slice i (i + n)

incrementASCIIBy :: Int -> Int -> Int
incrementASCIIBy x 0 = x
incrementASCIIBy x n = if next > topASCII then bottomASCII else next
  where next = x + n

incrementPass :: String -> String
incrementPass s = S.fromCharArray <<< A.reverse $ toEnumASCII <$> (go [] 1 (S.length s) $ A.reverse $ toInts s)
  where
    go acc carry n xs
      | carry == 0 || n < 1 = acc <> xs
      | otherwise = case A.head xs of
        Just x ->
          let next = incrementASCIIBy x carry in
          go (acc <> [next]) (if next == bottomASCII then 1 else 0) (n - 1) (fromMaybe [] $ A.tail xs)
        Nothing -> acc <> xs

hasThreeIncrement :: String -> Boolean
hasThreeIncrement s = 0 < A.length do
  index <- A.range 0 $ A.length xs - 3
  let
    threes = getNs 3 index xs
    firsts = A.concat $ A.replicate 3 $ A.take 1 threes
  guard $ [0, 1, 2] == A.zipWith (-) threes firsts
  pure $ unit
  where xs = toInts s

hasNoIOL :: String -> Boolean
hasNoIOL s = not A.any identity $ flip S.contains s <$> S.Pattern <$> ["i", "o", "l"]

hasTwoDifferentPairs :: String -> Boolean
hasTwoDifferentPairs s =
  A.all (\g -> NEA.length g < 3) xs && (A.length $ A.nub $ A.concat $ NEA.take 1 <$> A.filter (\g -> NEA.length g == 2) xs) > 1
  where xs = A.group $ toInts s


part1 :: String -> String
part1 = go <<< incrementPass
  where go s
          | (hasThreeIncrement && hasNoIOL && hasTwoDifferentPairs) s = s
          | otherwise = go $ incrementPass s
