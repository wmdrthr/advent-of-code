module Day06 (
  day6,
  solve
  ) where

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

redistribute :: V.Vector Int -> V.Vector Int
redistribute v = V.accum (+) v' [ (i `mod` V.length v, 1)
                                  | i <- [idx + 1 .. idx + blocks]]
  where idx    = V.maxIndex v
        blocks = v V.! idx
        v'     = v V.// [(idx, 0)]

findLoop :: [V.Vector Int] -> (Int, Int)
findLoop = next 0 M.empty
  where
    next _ _ [] = error "world gone mad!"
    next n m (x:xs) = case M.lookup x m of
                        Just l -> (n, l)
                        Nothing -> next (n + 1) (M.insert x 1 m') xs
      where m' = succ <$> m

solve :: [Int] -> (Int, Int)
solve input = findLoop $ iterate redistribute (V.fromList input)

day6 :: [Int] -> IO ()
day6 input = do
  print $ solve input
