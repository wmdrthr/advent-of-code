module RambunctiousRecitation where

import Data.Map.Strict as M

memoryGame :: Int -> [Int] -> Int
memoryGame limit numbers = go (M.fromList (zip numbers [0..])) (last numbers) (length numbers)
  where go :: M.Map Int Int -> Int -> Int -> Int
        go history number turn
          | turn == limit = number
          | otherwise     = case M.lookup number history of
                              Just n  -> go (M.insert number (pred turn) history) (turn - n - 1) (turn + 1)
                              Nothing -> go (M.insert number (pred turn) history) 0              (turn + 1)

day15 :: [Int] -> IO ()
day15 input = do
  print $ memoryGame 2020 input
  print $ memoryGame 30000000 input
