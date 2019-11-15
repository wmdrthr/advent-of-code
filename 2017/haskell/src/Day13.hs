{-# LANGUAGE ViewPatterns #-}

module Day13 (
  day13,
  solve1,
  solve2
  ) where

import Data.Char       (isDigit)
import Data.Foldable   (find)
import Data.Maybe      (fromJust)

scanner :: Int -> (Int, Int) -> Int
scanner delay (depth, range) = triangle (range - 1) (depth + delay)
  where
    triangle n x = abs ((x - n) `mod` (n * 2) - n)

caught :: Int -> (Int, Int) -> Bool
caught delay layer = scanner delay layer == 0

parse :: [String] -> [(Int, Int)]
parse = map parseLine
  where parseLine (words->x:n:_) = (read (filter isDigit x), read n)

solve1 :: [String] -> Int
solve1 = sum . map (uncurry (*)) . filter (caught 0) . parse

solve2 :: [String] -> Int
solve2 input = fromJust $ find neverCaught [0..]
  where
    neverCaught delay = all (not . caught delay) firewall
    firewall          = parse input

day13 :: [String] -> IO ()
day13 input = do
  print $ solve1 input
  print $ solve2 input
