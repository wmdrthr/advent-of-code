module HydrothermalVenture where

import qualified Data.Map        as M
import           Data.List.Split (splitOn)

type V2 = (Int, Int)

parse :: String -> (V2, V2)
parse line = ((x1, y1), (x2, y2))
  where (a:_:b:[]) = words line
        (x1:y1:[]) = (map read) $ splitOn "," a
        (x2:y2:[]) = (map read) $ splitOn "," b

generatePoints :: (V2, V2) -> [V2]
generatePoints ((x1, y1), (x2, y2)) = map nextPoint [0..length]
  where nextPoint d = (x1 + d * stepX, y1 + d * stepY)
        stepX  = dx `div` length
        stepY  = dy `div` length
        length = max (abs dx) (abs dy)
        dx     = x2 - x1
        dy     = y2 - y1

markPoint :: M.Map V2 Int -> V2 -> M.Map V2 Int
markPoint grid point = case M.lookup point grid of
  Nothing -> M.insert point 1 grid
  Just v  -> M.adjust succ point grid

solve :: [(V2, V2)] -> Int
solve lines = M.size
              $ M.filter (> 1)
              $ foldl markPoint M.empty (concatMap generatePoints lines)

solve1 :: [(V2, V2)] -> Int
solve1 = solve . filter (\((x1, y1),(x2,y2)) -> x1 == x2 || y1 == y2)

day05:: [String] -> IO ()
day05 input = do
  let lines = map parse input
  print $ solve1 lines
  print $ solve lines
