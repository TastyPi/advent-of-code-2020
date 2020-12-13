{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Advent2020 (withInput)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (mfilter)
import Data.Attoparsec.Text.Lazy
  ( Parser,
    Result,
    char,
    decimal,
    endOfInput,
    maybeResult,
    parse,
    string,
    takeText,
  )
import qualified Data.Attoparsec.Text.Lazy as A
import Data.Char (isHexDigit, isDigit)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text.Lazy (Text, splitOn)
import Passport (Passport (..), parseport)

parsepart1 :: Parser (Passport () () () () () () () ())
parsepart1 =
  parseport $
    Passport
      { _byr = (Nothing, takeText $> ()),
        _iyr = (Nothing, takeText $> ()),
        _eyr = (Nothing, takeText $> ()),
        _hgt = (Nothing, takeText $> ()),
        _hcl = (Nothing, takeText $> ()),
        _ecl = (Nothing, takeText $> ()),
        _pid = (Nothing, takeText $> ()),
        _cid = (Just (), takeText $> ())
      }

parsepart2 :: Integral a => Parser (Passport a a a (Height a) T.Text T.Text T.Text ())
parsepart2 =
  parseport $
    Passport
      { _byr = (Nothing, decimalInRange 1920 2002),
        _iyr = (Nothing, decimalInRange 2010 2020),
        _eyr = (Nothing, decimalInRange 2020 2030),
        _hgt = (Nothing, height),
        _hcl = (Nothing, hairColor),
        _ecl = (Nothing, eyeColor),
        _pid = (Nothing, passportId),
        _cid = (Just (), takeText $> ())
      }

decimalInRange :: Integral a => a -> a -> Parser a
decimalInRange lower upper = mfilter (\x -> lower <= x && x <= upper) decimal

data Height a = Centimetres a | Inches a deriving Show

height :: Integral a => Parser (Height a)
height =
  (Centimetres <$> decimalInRange 150 193 <* string "cm")
    <|> (Inches <$> decimalInRange 59 76 <* string "in")

hairColor :: Parser T.Text
hairColor = char '#' *> mfilter (T.all isHexDigit) (A.take 6)

eyeColor :: Parser T.Text
eyeColor =
  string "amb"
    <|> string "blu"
    <|> string "brn"
    <|> string "gry"
    <|> string "grn"
    <|> string "hzl"
    <|> string "oth"

passportId :: Parser T.Text
passportId = mfilter (T.all isDigit) (A.take 9)

parseports ::
  Parser (Passport byr eyr iyr hgt hcl ecl pid cid) ->
  Text ->
  [Result (Passport byr eyr iyr hgt hcl ecl pid cid)]
parseports parseporter =
  map (parse (parseporter <* endOfInput)) . splitOn "\n\n"

countValid :: [Result r] -> Int
countValid = length . mapMaybe maybeResult

main :: IO ()
main = withInput "data/day4/input.txt" $ \input -> do
  putStrLn $ "Part 1: " ++ show (countValid $ parseports parsepart1 input)
  putStrLn $ "Part 2: " ++ show (countValid $ parseports (parsepart2 @Int) input)
