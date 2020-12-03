{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Advent2020 (withParsedInputLines)
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    decimal,
    string,
    takeText,
  )
import Data.Range (inRange, (+=+))
import Data.Text (Text, count, index, singleton)

type Password = Text

data Policy = Policy {x :: Int, y :: Int, c :: Char}
  deriving (Show)

policy :: Parser Policy
policy = Policy <$> decimal <*> (char '-' *> decimal) <*> (char ' ' *> anyChar)

password :: Parser Password
password = takeText

policyAndPassword :: Parser (Policy, Password)
policyAndPassword = (,) <$> policy <*> (string ": " *> password)

satisfiesPart1 :: Policy -> Password -> Bool
satisfiesPart1 Policy {..} = inRange (x +=+ y) . count (singleton c)

satisfiesPart2 :: Policy -> Password -> Bool
satisfiesPart2 Policy {..} p = (index p (x -1) == c) /= (index p (y -1) == c)

main :: IO ()
main = withParsedInputLines "data/day2/input.txt" policyAndPassword $ \input -> do
  putStrLn $ "Part 1: " ++ show (length $ filter (uncurry satisfiesPart1) input)
  putStrLn $ "Part 2: " ++ show (length $ filter (uncurry satisfiesPart2) input)