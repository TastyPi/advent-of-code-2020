{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent2020 (withParsedInput)
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (ASetter, makeFields, set, (^.))
import Data.Attoparsec.Text (Parser, char, sepBy, sepBy1, space, string)
import qualified Data.Attoparsec.Text as A
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)

data PassportInput = PassportInput
  { _passportInputByr :: Maybe Text,
    _passportInputIyr :: Maybe Text,
    _passportInputEyr :: Maybe Text,
    _passportInputHgt :: Maybe Text,
    _passportInputHcl :: Maybe Text,
    _passportInputEcl :: Maybe Text,
    _passportInputPid :: Maybe Text,
    _passportInputCid :: Maybe Text
  }

data Passport = Passport
  { _passportByr :: Text,
    _passportIyr :: Text,
    _passportEyr :: Text,
    _passportHgt :: Text,
    _passportHcl :: Text,
    _passportEcl :: Text,
    _passportPid :: Text,
    _passportCid :: Maybe Text
  }

makeFields ''PassportInput
makeFields ''Passport

keyValue :: ASetter s t a b -> Text -> Parser b -> Parser (s -> t)
keyValue setter k v = set <$> (string k $> setter) <*> (char ':' *> v)

passportInputKeyValue :: Parser (PassportInput -> PassportInput)
passportInputKeyValue =
  keyValue byr "byr" simpleValue
    <|> keyValue iyr "iyr" simpleValue
    <|> keyValue eyr "eyr" simpleValue
    <|> keyValue hgt "hgt" simpleValue
    <|> keyValue hcl "hcl" simpleValue
    <|> keyValue ecl "ecl" simpleValue
    <|> keyValue pid "pid" simpleValue
    <|> keyValue cid "cid" simpleValue
  where
    simpleValue = pure <$> A.takeTill isSpace

maybePassport :: Parser PassportInput
maybePassport =
  foldl
    (flip id)
    (PassportInput Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    <$> passportInputKeyValue `sepBy1` space

maybePassports :: Parser [PassportInput]
maybePassports = maybePassport `sepBy` string "\n\n"

validatePassport :: PassportInput -> Maybe Passport
validatePassport p =
  Passport
    <$> (p ^. byr)
    <*> (p ^. iyr)
    <*> (p ^. eyr)
    <*> (p ^. hgt)
    <*> (p ^. hcl)
    <*> (p ^. ecl)
    <*> (p ^. pid)
    <*> pure (p ^. cid)

validPassports :: Parser [Passport]
validPassports = mapMaybe validatePassport <$> maybePassports

main :: IO ()
main = withParsedInput "data/day4/input.txt" validPassports $ \passports -> do
  putStrLn $ "Part 1: " ++ show (length passports)