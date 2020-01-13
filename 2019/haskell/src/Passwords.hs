module Passwords (valid1, valid2, day04) where

import           Control.Monad   (liftM2)
import           Data.List       (sort)
import           AdventOfCode    (cardinality)
import qualified Data.Map.Strict as M

increasing :: String -> Bool
increasing password = password == (sort password)

duplicates :: String -> Bool
duplicates password = (length candidates) > 0
  where candidates = filter (> 1) (M.elems $ cardinality password)

twins :: String -> Bool
twins password = (length candidates) > 0
  where candidates = filter (== 2) (M.elems $ cardinality password)

valid1 :: String -> Bool
valid1 = liftM2 (&&) increasing duplicates

valid2 :: String -> Bool
valid2 = liftM2 (&&) increasing twins

day04 :: [Int] -> IO ()
day04 (start:end:_) = do
  let passwords = map show [start ..end - 1]
  print $ length $ filter valid1 passwords
  print $ length $ filter valid2 passwords
