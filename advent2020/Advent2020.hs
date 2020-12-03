{-# LANGUAGE LambdaCase #-}

module Advent2020 (withInputLines, withParsedInputLines) where

import Control.Monad ((>=>))
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text)
import Data.Text.Lazy (lines, toStrict)
import Data.Text.Lazy.IO (hGetContents)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadMode), hSetBuffering, withFile)

withInputLines :: String -> ([Text] -> IO a) -> IO a
withInputLines file evaluate = withFile file ReadMode $ readLines >=> evaluate

withParsedInputLines :: String -> Parser a -> ([a] -> IO b) -> IO b
withParsedInputLines file parser evaluate =
  withInputLines file (evaluate . fmap (either error id . parseOnly parser))

readLines :: Handle -> IO [Text]
readLines handle =
  hSetBuffering handle LineBuffering
    >> map toStrict . Data.Text.Lazy.lines <$> hGetContents handle
