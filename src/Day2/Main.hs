{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day2.Main where

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
import Data.Range (Range, inRange, (+=+))
import Data.Text (Text, count, singleton)
import Data.Text.IO (hGetContents)
import System.IO (IOMode (ReadMode), withFile)

type Password = Text

type Policy = Password -> Bool

range :: Integral a => Parser (Range a)
range = do
    l <- decimal
    _ <- char '-'
    u <- decimal
    return $ l +=+ u

policy :: Parser Policy
policy = do
    r <- range
    _ <- char ' '
    x <- anyChar
    return $ inRange r . count (singleton x)

password :: Parser Password
password = takeTill isEndOfLine

policyAndPassword :: Parser (Policy, Password)
policyAndPassword = (,) <$> policy <*> (string ": " *> password)

withInput :: (String -> IO a) -> ([(Policy, Password)] -> IO a) -> IO a
withInput onError onSuccess =
  withFile "data/Day2/input.txt" ReadMode $
    hGetContents >=> (either onError onSuccess . parseOnly (policyAndPassword `sepBy` char '\n'))

main :: IO ()
main = withInput putStrLn $ \input -> do
  putStrLn $ "Part 1: " ++ show (length $ filter (uncurry id) input)