module PasswordPhilosophy where

import AdventOfCode          (count, xor)

type Entry = (String, Char, Int, Int)

valid1 :: Entry -> Bool
valid1 (password, letter, min, max) = min <= c && c <= max
  where c = count (letter==) password

valid2 :: Entry -> Bool
valid2 (password, letter, a, b) = fa `xor` fb
  where fa = (password !! (pred a)) == letter
        fb = (password !! (pred b)) == letter

parseEntry :: String -> Entry
parseEntry line = (p, head l, read a :: Int, read b :: Int)
  where (a:b:l:p:[]) = words [if x `elem` ":-" then ' ' else x | x <- line]


day02 :: [String] -> IO ()
day02 input = do
  let entries = map parseEntry input
  print $ length $ filter valid1 entries
  print $ length $ filter valid2 entries
