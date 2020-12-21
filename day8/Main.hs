{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Advent2020 (withParsedInputLines)
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makeLenses, over, view, (^.))
import Control.Monad.State (MonadState (get, put), execState)
import Data.Array (Array, listArray, (!))
import Data.Attoparsec.Text.Lazy (Parser, decimal, signed, string)
import qualified Data.IntSet as IntSet

data OpCode = Acc Int | Jmp Int | Nop Int deriving (Show)

opCode :: Parser OpCode
opCode =
  (Acc <$> (string "acc " *> signed decimal))
    <|> (Jmp <$> (string "jmp " *> signed decimal))
    <|> (Nop <$> (string "nop " *> signed decimal))

data VMState = VMState {_ptr :: Int, _acc :: Int}

makeLenses ''VMState

runOpCode :: OpCode -> VMState -> VMState
runOpCode (Acc x) = over acc (+ x) . over ptr (+ 1)
runOpCode (Jmp x) = over ptr (+ x)
runOpCode (Nop _) = over ptr (+ 1)

part1 :: Array Int OpCode -> Int
part1 ops = view acc . fst $ execState run (VMState 0 0, IntSet.empty)
  where
    run = do
      (vm, visited) <- get
      let vm' = runOpCode (ops ! (vm ^. ptr)) vm
      if IntSet.member (vm' ^. ptr) visited
        then return $ vm' ^. acc
        else do
          put (vm', IntSet.insert (vm' ^. ptr) visited)
          run

main :: IO ()
main = withParsedInputLines "data/day8/input.txt" opCode $ \input -> do
  putStrLn $ "Part 1: " ++ show (part1 $ listArray (0, length input) input)