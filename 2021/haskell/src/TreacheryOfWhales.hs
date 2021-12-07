module TreacheryOfWhales where

import Data.List   (sort, minimum)
import Debug.Trace

median :: [Int] -> Int
median xs
  | odd  len = xs !! mid
  | even len = (xs !! (mid-1) + xs !! mid) `quot` 2
    where len = length xs
          mid = len `div` 2

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
