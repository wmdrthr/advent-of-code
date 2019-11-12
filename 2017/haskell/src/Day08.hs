{-# LANGUAGE ViewPatterns  #-}

module Day08 (
  day8,
  solve
  ) where

import Prelude hiding (LT, GT, EQ)

import Debug.Trace

import Text.Printf
import qualified Data.Map.Strict as M

data Operation = Inc | Dec deriving (Eq, Show)
data Operator = GT | GE | LT | LE | EQ | NEQ deriving (Eq, Show)
type Register = String
data Instruction = Instruction { mainRegister :: Register,
                                 operation :: Operation,
                                 value :: Int,
                                 conditionRegister :: Register,
                                 operator :: Operator,
                                 conditionValue :: Int
                               } deriving (Show)

data Vals = Vals { current :: Int,
                   highest :: Int
                 } deriving (Show, Eq)
type Memory = M.Map Register Vals

makeOperation :: String -> Operation
makeOperation "inc" = Inc
makeOperation "dec" = Dec
makeOperation _     = error "invalid operation"

parseLine :: String -> Instruction
parseLine (words->r:o:v:_:cr:op:cv:[]) = Instruction r operation (read v) cr operator (read cv)
  where operation = case o of
          "inc" -> Inc
          "dec" -> Dec
        operator = case op of
          ">"  -> GT
          ">=" -> GE
          "<"  -> LT
          "<=" -> LE
          "==" -> EQ
          "!=" -> NEQ

get :: Memory -> String -> Int
get m r = current $ M.findWithDefault (Vals 0 0)  r m

set :: Memory -> String -> (Int -> Int) -> Memory
set m r f = let c = get m r
                v = f c
            in M.insert r (Vals v (max v c)) m

evaluate :: Memory -> Instruction -> Bool
evaluate m (Instruction _ _ _ cr GT cv)  = (get m cr) > cv
evaluate m (Instruction _ _ _ cr LT cv)  = (get m cr) < cv
evaluate m (Instruction _ _ _ cr EQ cv)  = (get m cr) == cv
evaluate m (Instruction _ _ _ cr NEQ cv) = (get m cr) /= cv
evaluate m (Instruction _ _ _ cr GE cv)  = (get m cr) >= cv
evaluate m (Instruction _ _ _ cr LE cv)  = (get m cr) <= cv

execute :: Memory -> Instruction -> Memory
execute m i@(Instruction r Inc v _ _ _) = set m r (\x -> x + v)
execute m i@(Instruction r Dec v _ _ _) = set m r (\x -> x - v)

next :: Memory -> Instruction -> Memory
next m instr
  | evaluate m instr == True = execute m instr
  | otherwise                = m

solve :: [String] -> (Int, Int)
solve input = (maximum (map current (M.elems result)),
               maximum (map highest (M.elems result)))
  where result = foldl next M.empty (map parseLine input)

day8 :: [String] -> IO ()
day8 input = do
  let result = solve input
  print $ (fst result)
  print $ (snd result)
