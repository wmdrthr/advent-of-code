module Tsiolkovsky (solve1, solve2, day01) where

fuel :: Int -> Int
fuel mass = (floor (fromIntegral mass / 3)) - 2

extraFuel :: Int -> Int
extraFuel = sum . takeWhile (> 0) . drop 1 . iterate fuel

solve1 :: [Int] -> Int
solve1 = sum . map fuel

solve2 :: [Int] -> Int
solve2 = sum . map extraFuel

day01 :: [String] -> IO ()
day01 input = do
  print $ solve1 (map read input)
  print $ solve2 (map read input)
