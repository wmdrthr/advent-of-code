module Day02 (
  day2,
  solve1,
  solve2
  ) where

import Data.List

solve :: ([Int] -> Int) -> [[Int]] -> Int
solve rowChecksum table = sum $ map rowChecksum table

part1Checksum :: [Int] -> Int
part1Checksum row = (maximum row) - (minimum row)

combinations :: Int -> [Int] -> [[Int]]
combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

part2Checksum :: [Int] -> Int
part2Checksum row = head $ (map (\[a, b] -> a `div` b) divisibles)
  where sortedRow = reverse $ sort row
        divisibles = filter (\[a, b] -> a `mod` b == 0) (combinations 2 sortedRow)

solve1 = solve part1Checksum
solve2 = solve part2Checksum

day2 :: [[Int]] -> IO ()
day2 input = do
  print $ solve1 input
  print $ solve2 input
