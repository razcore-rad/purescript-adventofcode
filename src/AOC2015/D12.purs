module AOC2015.D12 where

import Data.Argonaut.Parser (jsonParser)


part1 = jsonParser """{"a":1, "b":2}"""
