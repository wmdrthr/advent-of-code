module ReportRepair where

import AdventOfCode (combinations)

solve :: Int -> [Int] -> Int
solve k xs = product $ head $ filter ((2020==).sum) $ combinations k xs



day01 :: [Int] -> IO ()
day01 input = do
  print $ solve 2 input
  print $ solve 3 input
