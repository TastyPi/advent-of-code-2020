{-# LANGUAGE OverloadedStrings #-}

module Main where

import Advent2020 (withInput)
import Data.Set (fromList)
import Data.Text.Lazy (unpack)
import qualified Data.Text.Lazy as T

main :: IO ()
main = withInput "data/day6/input.txt" $ \input -> do
  let groups = map T.lines $ T.splitOn "\n\n" input
  putStrLn $ "Part 1: " ++ show (sum $ map (length . foldMap (fromList . unpack)) groups)