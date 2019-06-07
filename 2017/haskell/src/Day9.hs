module Day9 (
  day9,
  solve1,
  solve2
  ) where

import Data.List (mapAccumL)

solve1 :: String -> Int
solve1 = sum . snd . mapAccumL accum (1, Nothing) where
  accum (k,    Nothing) '{' = ((k + 1,    Nothing), k)
  accum (k,    Nothing) '}' = ((k - 1,    Nothing), 0)
  accum (k,    Nothing) '<' = ((k,     Just False), 0)
  accum (k, Just False) '>' = ((k,        Nothing), 0)
  accum (k, Just False) '!' = ((k,      Just True), 0)
  accum (k,  Just True)  _  = ((k,     Just False), 0)
  accum acc _ = (acc, 0)

solve2 = sum . snd . mapAccumL accum Nothing where
  accum Nothing     '<' = (Just True,  0)
  accum Nothing      _  = (Nothing,    0)
  accum (Just True) '>' = (Nothing,    0)
  accum (Just True) '!' = (Just False, 0)
  accum (Just True)  _  = (Just True,  1)
  accum (Just False) _  = (Just True,  0)

day9 :: String -> IO ()
day9 input = do
  print $ solve1 input
  print $ solve2 input
