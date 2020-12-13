{-# LANGUAGE TypeApplications #-}
module Main where

import Advent2020 (withInputLines)
import Control.Arrow ((***))
import Data.Text (Text, unpack)
import qualified Data.Text as T

boardingPassToId :: Integral a => Text -> a
boardingPassToId t = uncurry (+) $ (((8 *) . findRow) *** findCol) $ T.splitAt 7 t

findRow :: Integral a => Text -> a
findRow = findIndex 'F' 'B'

findCol :: Integral a => Text -> a
findCol = findIndex 'L' 'R'

findIndex :: Integral a => Char -> Char -> Text -> a
findIndex bottom top text = go 0 (2 ^ T.length text) (unpack text)
  where
    go l _ [] = l
    go l u (c : cs)
      | c == bottom = go l average cs
      | c == top = go average u cs
      | otherwise = error $ "Unrecognized boarding pass character " ++ show c
      where
        average = (l + u) `div` 2

main :: IO ()
main = withInputLines "data/day5/input.txt" $ \input -> do
  putStrLn $ "Part 1: " ++ show (maximum $ map (boardingPassToId @Int) input)
