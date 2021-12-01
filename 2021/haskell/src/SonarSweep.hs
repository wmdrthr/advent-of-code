module SonarSweep where

calculate :: [Int] -> Int
calculate l = length $ filter (\(a, b) -> b > a) (zip l (tail l))

sums :: [Int] -> [Int]
sums l = map (\(a, b, c) -> a + b + c) (zip3 l (tail l) (drop 2 l))

day01 :: [Int] -> IO ()
day01 input = do
  print $ calculate input
  print $ calculate (sums input)
