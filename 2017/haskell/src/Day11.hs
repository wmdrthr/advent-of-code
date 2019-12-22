module Day11 (
  day11,
  solve
  ) where

import Data.List.Split    (splitOn)

data Hex = Hex { x :: Int, y ::Int, z :: Int } deriving (Show, Eq)
data Direction = N | NE | SE | S | SW | NW deriving (Eq, Show)

direction :: String -> Direction
direction d = case d of
               "n"  -> N
               "ne" -> NE
               "se" -> SE
               "s"  -> S
               "sw" -> SW
               "nw" -> NW
               _    -> error "invalid direction"

step :: Hex -> Direction -> Hex
step (Hex a b c) N  = Hex a (succ b) (pred c)
step (Hex a b c) NE = Hex (succ a) b (pred c)
step (Hex a b c) SE = Hex (succ a) (pred b) c
step (Hex a b c) S  = Hex a (pred b) (succ c)
step (Hex a b c) SW = Hex (pred a) b (succ c)
step (Hex a b c) NW = Hex (pred a) (succ b) c

walk :: String -> [Hex]
walk path = scanl step (Hex 0 0 0) $ map direction $ splitOn "," path

distance :: Hex -> Hex -> Int
distance (Hex a1 b1 c1) (Hex a2 b2 c2) = maximum $ map abs offsets
  where offsets = [a1 - a2] ++ [b1 - b2] ++ [c1 - c2]

solve :: String -> (Int, Int)
solve input = (distance (Hex 0 0 0) (last path),
               maximum $ map (distance (Hex 0 0 0)) path)
  where path = walk input

day11 :: String -> IO ()
day11 input = do
  print $ solve input
