module ChronalCalibration (
  chronalCalibration,
  solve1,
  solve2
  ) where

import AdventOfCode

import Data.Maybe (fromJust)

import qualified Data.Set as Set

solve1 :: [Int] -> Int
solve1 input = foldl (+) 0 input

firstRepetition :: [Int] -> Maybe Int
firstRepetition = go Set.empty
  where go _ [] = Nothing
        go mem (x:xs)
          | x `Set.member` mem = Just x
          | otherwise          = go (Set.insert x mem) xs

solve2 :: [Int] -> Int
solve2 = fromJust . firstRepetition . partialSums . cycle
  where partialSums = scanl (+) 0

chronalCalibration :: [Int] -> IO ()
chronalCalibration input = do
  print $ solve1 input
  print $ solve2 input
