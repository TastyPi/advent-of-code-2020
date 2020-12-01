{-# LANGUAGE TypeApplications #-}
module Day1.Main where

import Control.Monad
import System.IO

pairs :: [a] -> [[a]]
pairs [] = []
pairs (x:xs) = map (\y -> [x, y]) xs ++ pairs xs

triples :: [a] -> [[a]]
triples [] = []
triples (x:xs) = map (x:) (pairs xs) ++ triples xs

findSolutions :: (Eq a, Num a) => [[a]] -> [a]
findSolutions = map product . filter ((== 2020) . sum)

computePart1 :: (Eq a, Num a) => [a] -> [a]
computePart1 = findSolutions . pairs

computePart2 :: (Eq a, Num a) => [a] -> [a]
computePart2 = findSolutions . triples

withInput :: Read a => ([a] -> IO b) -> IO b
withInput f = withFile "data/Day1/input.txt" ReadMode $ hGetContents >=> (f . map read . lines)

main :: IO ()
main = withInput @Int $ \input -> do
  putStrLn $ "Part 1: " ++ show (computePart1 input)
  putStrLn $ "Part 2: " ++ show (computePart2 input)
