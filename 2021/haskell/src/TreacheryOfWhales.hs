module TreacheryOfWhales where

import Data.List   (sort, minimum)
import AdventOfCode

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

solve1 :: [Int] -> Int
solve1 xs = sum $ map (\n -> abs (n - m)) xs
  where m = median xs

solve2 :: [Int] -> Int
solve2 xs = minimum $ map cost [(m-1)..(m+1)]
  where cost p = sum $ map (\n -> fuel (abs (n - p))) xs
        fuel c = (c * (c + 1)) `div` 2
        m      = mean xs

day07 :: [Int] -> IO ()
day07 input = do
  let positions = sort input
  print $ solve1 positions
  print $ solve2 positions
