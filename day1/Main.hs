{-# LANGUAGE TypeApplications #-}

module Main where

import Advent2020 (withInput)
import Data.Attoparsec.Text (Parser, decimal)

pairs :: [a] -> [[a]]
pairs [] = []
pairs (x : xs) = map (\y -> [x, y]) xs ++ pairs xs

triples :: [a] -> [[a]]
triples [] = []
triples (x : xs) = map (x :) (pairs xs) ++ triples xs

isSolution :: (Eq a, Num a) => [a] -> Bool
isSolution = (== 2020) . sum

findSolutions :: (Eq a, Num a) => [[a]] -> [a]
findSolutions = map product . filter isSolution

computePart1 :: (Eq a, Num a) => [a] -> [a]
computePart1 = findSolutions . pairs

computePart2 :: (Eq a, Num a) => [a] -> [a]
computePart2 = findSolutions . triples

entry :: Integral a => Parser a
entry = decimal

main :: IO ()
main = withInput "data/day1/input.txt" (entry @Int) $ \entries -> do
  putStrLn $ "Part 1: " ++ show (computePart1 entries)
  putStrLn $ "Part 2: " ++ show (computePart2 entries)
