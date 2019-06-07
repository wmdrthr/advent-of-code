module Day1 (
  day1,
  solve1,
  solve2
  ) where

solve :: Int -> [Int] -> Int
solve n xs = sum $ zipWith (\a b -> if a == b then a else 0) xs (drop n $ cycle xs)

solve1 = solve 1
solve2 xs = solve ((length xs) `div` 2) xs

day1 :: [Int] -> IO ()
day1 input = do
  print $ solve1 input
  print $ solve2 input
