{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module SeatingSystem where

import qualified Data.Map as M
import Data.Maybe         (mapMaybe)
import Prelude            hiding (Left, Right)

type V2 = (Int, Int)
data Seat = Floor | Empty | Occupied deriving (Show, Eq)

data Layout = Layout { grid :: M.Map V2 Seat,
                       nRows :: Int,
                       nCols :: Int
                     } deriving (Show)

data Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
directions = [Up, Down, Left, Right, UpLeft, UpRight, DownLeft, DownRight]

next :: Layout -> V2 -> Direction -> Maybe V2
next _             (x,y) Up    = if y > 0          then Just (x, y - 1) else Nothing
next Layout{nRows} (x,y) Down  = if y + 1 < nRows  then Just (x, y + 1) else Nothing
next _             (x,y) Left  = if x > 0          then Just (x - 1, y) else Nothing
next Layout{nCols} (x,y) Right = if x + 1 < nCols  then Just (x + 1, y) else Nothing
next _             (x,y) UpLeft    = if x > 0 && y > 0         then Just (x - 1, y - 1) else Nothing
next Layout{nCols} (x,y) UpRight   = if x + 1 < nCols && y > 0 then Just (x + 1, y - 1) else Nothing
next Layout{nRows} (x,y) DownLeft  = if x > 0 && y + 1 < nRows then Just (x - 1, y + 1) else Nothing
next layout        (x,y) DownRight = if x + 1 < (nCols layout) && y + 1 < (nRows layout)
                                     then Just (x + 1, y + 1)
                                     else Nothing

neighbors :: Layout -> V2 -> [V2]
neighbors layout point = mapMaybe (next layout point) directions


nearestNeighbors :: Layout -> V2 -> [V2]
nearestNeighbors layout point = mapMaybe (scan layout point) directions
  where scan :: Layout -> V2 -> Direction -> Maybe V2
        scan Layout{grid} point direction = case (next layout point direction) of
                                              Just p -> if grid M.! p /= Floor
                                                        then Just p
                                                        else scan layout p direction
                                              Nothing -> Nothing

neighboringSeats :: (Seat -> Bool) -> Layout -> V2 -> (Layout -> V2 -> [V2]) -> Int
neighboringSeats seatFn layout pos neighborFn = length
                                                $ filter seatFn
                                                $ map (\n -> (grid layout) M.! n)
                                                $ neighborFn layout pos
emptySeats    = neighboringSeats (/=Occupied)
occupiedSeats = neighboringSeats (==Occupied)

step :: Layout -> (Layout -> V2 -> [V2]) -> Int -> Layout
step layout@Layout{grid,nRows,nCols} neighborFn maxNeighbors = Layout (M.mapWithKey calc grid) nRows nCols
  where calc pos Empty
          | (emptySeats layout pos neighborFn) == (length $ neighborFn layout pos) = Occupied
          | otherwise = Empty
        calc pos Occupied
          | (occupiedSeats layout pos neighborFn) >= maxNeighbors = Empty
          | otherwise = Occupied
        calc _ Floor = Floor

solve :: (Layout -> V2 -> [V2]) -> Int -> Layout -> Int
solve neighborFn maxNeighbors layout = M.size $ M.filter (==Occupied) $ go layout
  where go layout = let newGrid = grid (step layout neighborFn maxNeighbors) in
                        if (grid layout) == newGrid
                        then newGrid
                        else go layout{grid = newGrid}

solve11a = solve neighbors 4
solve11b = solve nearestNeighbors 5

parse :: String -> Layout
parse input = Layout grid (length rows) (length (head rows))
  where grid = M.fromList $ zip indexes (map getSeat $ filter (\c -> c /= '\n') input)
        indexes = [(i, j) | j <- [0..(pred $ length rows)], i <- [0..(pred $ length (head rows))]]
        rows    = lines input
        getSeat '.' = Floor
        getSeat 'L' = Empty
        getSeat '#' = Occupied

day11 :: String  -> IO ()
day11 input = do
  let layout = parse input
  print $ solve11a layout
  print $ solve11b layout
