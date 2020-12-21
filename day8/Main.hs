{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Advent2020 (withParsedInputLines)
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (makeLenses, over, view, (??), (^.), _1)
import Control.Monad (MonadPlus (mzero), guard, mfilter)
import Control.Monad.Logic (Logic, MonadLogic (interleave), observeAll)
import Control.Monad.State (MonadState (put), StateT, evalStateT, gets)
import Data.Array (Array, Ix (inRange), listArray, (!))
import qualified Data.Array as Array
import Data.Attoparsec.Text.Lazy (Parser, decimal, signed, string)
import Data.Functor.Identity (Identity (Identity, runIdentity))
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

runProg :: Monad m => (OpCode -> m (VMState -> VMState)) -> Array Int OpCode -> m (VMState, Bool)
runProg eval ops = go IntSet.empty (VMState 0 0)
  where
    go visited vm
      | IntSet.member (vm ^. ptr) visited = pure (vm, False)
      | not (inRange (Array.bounds ops) (vm ^. ptr)) = pure (vm, True)
      | otherwise =
        eval (ops ! (vm ^. ptr)) ?? vm
          >>= go (IntSet.insert (vm ^. ptr) visited)

part1 :: Array Int OpCode -> Int
part1 = view (_1 . acc) . runIdentity . runProg (Identity . runOpCode)

part2 :: Array Int OpCode -> [Int]
part2 = map (view acc . fst) . observeAll . flip evalStateT False . mfilter snd . runProg eval
  where
    eval :: OpCode -> StateT Bool Logic (VMState -> VMState)
    eval op =
      return (runOpCode op) `interleave` do
        gets not >>= guard
        put True
        case op of
          Acc _ -> mzero
          Jmp x -> return $ runOpCode (Nop x)
          Nop x -> return $ runOpCode (Jmp x)

main :: IO ()
main = withParsedInputLines "data/day8/input.txt" opCode $ \input -> do
  let ops = listArray (0, length input - 1) input
  putStrLn $ "Part 1: " ++ show (part1 ops)
  putStrLn $ "Part 2: " ++ show (part2 ops)