{-# LANGUAGE LambdaCase #-}

module Advent2020 (withInput) where

import Control.Monad ((>=>))
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text.Lazy (lines, toStrict)
import Data.Text.Lazy.IO (hGetContents)
import System.IO (Handle, IOMode (ReadMode), withFile)

withInput :: String -> Parser a -> ([a] -> IO ()) -> IO ()
withInput file parser evaluate = withFile file ReadMode $ parseLines parser >=> evaluate

parseLines :: Parser a -> Handle -> IO [a]
parseLines parser handle = do
  contents <- hGetContents handle
  return $ either error id . parseOnly parser . toStrict <$> Data.Text.Lazy.lines contents
