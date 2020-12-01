{-# LANGUAGE TupleSections, TypeApplications #-}
module Day1.Main where

import System.IO

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

part1 :: IO Int
part1 = withFile "data/Day1/input.txt" ReadMode $ \handle -> do
  input <- hGetContents handle
  case filter (\(x, y) -> x + y == 2020) $ pairs $ map read $ lines input of
    [(x, y)] -> return $ x * y
    x -> error $ "Failed to find single pair: " ++ show x

main :: IO ()
main = part1 >>= print
