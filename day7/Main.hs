{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Advent2020 (withParsedInputLines)
import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State, evalState, gets, modify)
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    decimal,
    manyTill,
    option,
    sepBy,
    space,
    string,
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Multimap.Table (Table)
import qualified Data.Multimap.Table as Table
import Data.Text (Text, pack)
import Data.Tuple (swap)

type BagType = Text

type BagRules = Table BagType BagType Int

rule :: Parser (BagType, Map BagType Int)
rule = (,) <$> (pack <$> manyTill anyChar (string " bags contain ")) <*> innerRules

innerRules :: Parser (Map Text Int)
innerRules = Map.fromList <$> sepBy innerRule (string ", ")

innerRule :: Parser (Text, Int)
innerRule =
  fmap swap $
    (,)
      <$> (decimal <* space)
      <*> (pack <$> manyTill anyChar (string " bag" <* option 's' (char 's')))

part1 :: BagRules -> [BagType]
part1 rs = evalState (filterM (canContainShinyGold rs) (Table.rowKeys rs)) Map.empty

canContainShinyGold :: BagRules -> BagType -> State (Map BagType Bool) Bool
canContainShinyGold rs b = do
  s <- gets (Map.lookup b)
  case s of
    Just r -> return r
    Nothing -> case Table.lookup b "shiny gold" rs of
        Just _ -> do
            modify (Map.insert b True)
            return True
        Nothing -> do
            result <- or <$> mapM (canContainShinyGold rs) (Map.keys (Table.row b rs))
            modify (Map.insert b result)
            return result

main :: IO ()
main = withParsedInputLines "data/day7/input.txt" rule $ \input -> do
  let rules = Table.fromRowMap (Map.fromList input)
  putStrLn $ "Part 1: " ++ show (length $ part1 rules)
