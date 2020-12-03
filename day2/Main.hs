{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad ((>=>))
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    decimal,
    isEndOfLine,
    parseOnly,
    sepBy,
    string,
    takeTill,
  )
import Data.Range (inRange, (+=+))
import Data.Text (Text, count, index, singleton)
import Data.Text.IO (hGetContents)
import System.IO (IOMode (ReadMode), withFile)

type Password = Text

data Policy = Policy {x :: Int, y :: Int, c :: Char}

policy :: Parser Policy
policy = Policy <$> decimal <*> (char '-' *> decimal) <*> (char ' ' *> anyChar)

password :: Parser Password
password = takeTill isEndOfLine

policyAndPassword :: Parser (Policy, Password)
policyAndPassword = (,) <$> policy <*> (string ": " *> password)

satisfiesPart1 :: Policy -> Password -> Bool
satisfiesPart1 Policy {..} = inRange (x +=+ y) . count (singleton c)

satisfiesPart2 :: Policy -> Password -> Bool
satisfiesPart2 Policy {..} p = (index p (x -1) == c) /= (index p (y -1) == c)

withInput :: (String -> IO a) -> ([(Policy, Password)] -> IO a) -> IO a
withInput onError onSuccess =
  withFile "data/day2/input.txt" ReadMode $
    hGetContents >=> (either onError onSuccess . parseOnly (policyAndPassword `sepBy` char '\n'))

main :: IO ()
main = withInput putStrLn $ \input -> do
  putStrLn $ "Part 1: " ++ show (length $ filter (uncurry satisfiesPart1) input)
  putStrLn $ "Part 2: " ++ show (length $ filter (uncurry satisfiesPart2) input)