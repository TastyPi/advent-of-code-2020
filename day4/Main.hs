{-# LANGUAGE OverloadedStrings #-}

module Main where

import Advent2020 (withInput)
import Data.Attoparsec.Text.Lazy (takeText, maybeResult, Result, Parser, endOfInput, parse)
import Data.Functor (($>))
import Data.Text.Lazy (Text, splitOn)
import Passport (Passport (..), parseport)
import Data.Maybe (mapMaybe)

parsepart1 :: Parser (Passport () () () () () () () ())
parsepart1 =
  parseport $
    Passport
      { _byr = (Nothing, takeText $> ()),
        _eyr = (Nothing, takeText $> ()),
        _iyr = (Nothing, takeText $> ()),
        _hgt = (Nothing, takeText $> ()),
        _hcl = (Nothing, takeText $> ()),
        _ecl = (Nothing, takeText $> ()),
        _pid = (Nothing, takeText $> ()),
        _cid = (Just (), takeText $> ())
      }

parseports ::
  Parser (Passport byr eyr iyr hgt hcl ecl pid cid) ->
  Text ->
  [Result (Passport byr eyr iyr hgt hcl ecl pid cid)]
parseports parseporter =
  map (parse (parseporter <* endOfInput)) . splitOn ("\n\n" :: Text)

main :: IO ()
main = withInput "data/day4/input.txt" $ \input -> do
  putStrLn $ "Part1: " ++ show (length $ mapMaybe maybeResult $ parseports parsepart1 input)
