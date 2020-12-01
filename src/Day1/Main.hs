module Day1.Main where

import Control.Monad
import System.IO

pairs :: [a] -> [[a]]
pairs [] = []
pairs (x:xs) = map (\y -> [x, y]) xs ++ pairs xs

triples :: [a] -> [[a]]
triples [] = []
triples (x:xs) = map (x:) (pairs xs) ++ triples xs

onlyElement :: [a] -> a
onlyElement [x] = x
onlyElement _ = error $ "List does not have a single element"

findSolution :: (Eq a, Num a) => [[a]] -> a
findSolution = product . onlyElement . filter ((== 2020) . sum)

computePart1 :: (Eq a, Num a) => [a] -> a
computePart1 = findSolution . pairs

computePart2 :: (Eq a, Num a) => [a] -> a
computePart2 = findSolution . triples

type Entry = Int

withInput :: ([Entry] -> IO a) -> IO a
withInput f = withFile "data/Day1/input.txt" ReadMode $  (f . map read . lines) <=< hGetContents

main :: IO ()
main = withInput $ \input -> do
  putStrLn $ "Part 1: " ++ show (computePart1 input)
  putStrLn $ "Part 2: " ++ show (computePart2 input)
