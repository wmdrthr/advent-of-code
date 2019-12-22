{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumDecimals #-}

module Day15 (
  day15,
  solve1,
  solve2
  ) where

import Data.Word      (Word16)
import Data.Function  (on)

parse :: String -> Int
parse = read . last . words

generate :: Int -> Int -> Int
generate factor = (`mod` 2147483647) . (* factor)

factorA, factorB :: Int
factorA = 16807
factorB = 48271

countFirst :: Int -> [(Int, Int)] -> Int
countFirst n = length . filter (uncurry judge) . take n
  where
    judge = (==) @Word16 `on` fromIntegral

solve1 :: (Int, Int) -> Int
solve1 (seedA, seedB) = countFirst 4e7 $ zip (iterate (generate factorA) seedA)
                                             (iterate (generate factorB) seedB)

solve2 :: (Int, Int) -> Int
solve2 (seedA, seedB) = countFirst 5e6
                        $ zip (filter (`divBy` 4) . iterate (generate factorA) $ seedA)
                              (filter (`divBy` 8) . iterate (generate factorB) $ seedB)
  where
    x `divBy` b = x `mod`b == 0

day15 :: [String] -> IO ()
day15 input = do
  let seeds = ((parse . head) input, (parse . last) input)
  print $ solve1 seeds
  print $ solve2 seeds
