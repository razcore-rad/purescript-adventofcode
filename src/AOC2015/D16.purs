module AOC2015.D16 where

import Prelude

import Data.Array (catMaybes, filter, head, length, many, range, zip)
import Data.Either (fromRight)
import Data.Foldable (maximum, sum)
import Data.HashMap (HashMap)
import Data.HashMap as HM
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy)
import Parsing.Language (emptyDef)
import Parsing.String (anyTill, char)
import Parsing.Token (TokenParser, makeTokenParser)

tape :: HashMap String Int
tape = HM.fromFoldable
  [ "children" /\ 3
  , "cats" /\ 7
  , "samoyeds" /\ 2
  , "pomeranians" /\ 3
  , "akitas" /\ 0
  , "vizslas" /\ 0
  , "goldfish" /\ 5
  , "trees" /\ 3
  , "cars" /\ 2
  , "perfumes" /\ 1
  ]

getInput :: Aff String
getInput = readTextFile UTF8 "./data/aoc2015-d16"

tokenParser :: TokenParser
tokenParser = makeTokenParser emptyDef

parser :: Parser String (Array (HashMap String Int))
parser = map (map HM.fromFoldable) $ many $ (anyTill $ char ':') *> itemParser `sepBy` tokenParser.comma
  where
    itemParser = tokenParser.whiteSpace *> do
      key <- tokenParser.identifier <* tokenParser.colon
      value <- tokenParser.integer
      pure $ key /\ value

toMCFSAM :: String -> Array (HashMap String Int)
toMCFSAM s = fromRight [] $ runParser s parser

part1 :: Effect Unit
part1 = launchAff_ do
  input <- toMCFSAM <$> getInput
  let samScores = do
        item <- input
        pure $ sum $ map (const 1) $ filter identity $ catMaybes $ map (\k -> (==) <$> HM.lookup k item <*> HM.lookup k tape) $ HM.keys item
  logShow $ do
    m <- maximum samScores
    map fst $ head $ filter (\(_ /\ score) -> score == m) $ zip (range 1 $ length input) samScores
