{-# LANGUAGE LambdaCase #-}

module Advent2020
  ( withInput,
    withInputLines,
    withParsedInput,
    withParsedInputLines,
  )
where

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Attoparsec.Text.Lazy (Result (Done, Fail))
import qualified Data.Attoparsec.Text.Lazy as Lazy
import Data.Text (Text)
import Data.Text.Lazy (lines, toStrict, unpack)
import qualified Data.Text.Lazy as T.Lazy
import Data.Text.Lazy.IO (hGetContents)
import System.IO
  ( BufferMode (LineBuffering),
    Handle,
    IOMode (ReadMode),
    hSetBuffering,
    withFile,
  )

withInput :: String -> (T.Lazy.Text -> IO a) -> IO a
withInput file evaluate = withFile file ReadMode $ hGetContents >=> evaluate

withInputLines :: String -> ([Text] -> IO a) -> IO a
withInputLines file evaluate = withFile file ReadMode $ readLines >=> evaluate

withParsedInputLines :: String -> Parser a -> ([a] -> IO b) -> IO b
withParsedInputLines file parser evaluate =
  withInputLines file (evaluate . fmap (either error id . parseOnly parser))

readLines :: Handle -> IO [Text]
readLines handle =
  hSetBuffering handle LineBuffering
    >> map toStrict . Data.Text.Lazy.lines <$> hGetContents handle

withParsedInput :: String -> Lazy.Parser a -> (a -> IO b) -> IO b
withParsedInput file parser evaluate =
  withFile file ReadMode $
    hGetContents >=> Lazy.parse parser >>> \case
      Fail _ _ msg -> error msg
      Done remaining result ->
        if T.Lazy.null remaining
          then evaluate result
          else error ("Didn't parse everything:\n" ++ unpack remaining)
