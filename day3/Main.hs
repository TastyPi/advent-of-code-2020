{-# LANGUAGE RecordWildCards #-}

module Main where

import Advent2020 (withInputLines)
import Data.Text (Text, index)
import qualified Data.Text as Text

type Map = [Text]

type Square = Char

isTree :: Square -> Bool
isTree = (== '#')

data Slope = Slope {right :: Int, down :: Int}

slope :: Map -> Slope -> [Square]
slope m Slope {..} = go right down
  where
    go x y
      | length m <= y = []
      | otherwise =
        let line = m !! y in (line `index` (x `mod` Text.length line)) : go (x + right) (y + down)

treesOnSlope :: Map -> Slope -> Int
treesOnSlope m s = length $ filter isTree $ slope m s

main :: IO ()
main = withInputLines "data/day3/input.txt" $ \inputLines ->
  putStrLn $ "Part 1: " ++ show (treesOnSlope inputLines (Slope 3 1))