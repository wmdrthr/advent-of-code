module Day05 (
  day5,
  solve1,
  solve2
  ) where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

solve :: (Int -> Int) -> [Int] -> Int
solve change input = runST $ V.thaw offsets >>= next 0 0
  where offsets = V.fromList input
        next c i v
          | i < 0 || i >= M.length v = return c
          | otherwise = do
              val <- M.read v i
              M.write v i $ change val
              next (c + 1) (i + val) v

solve1 = solve succ
solve2 = solve (\x -> if x >= 3
                 then x - 1
                 else x + 1)

day5 :: [Int] -> IO ()
day5 input = do
  print $ solve1 input
  print $ solve2 input
