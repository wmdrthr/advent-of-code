module RainRisk where

import AdventOfCode

import Debug.Trace

type V2 = (Int, Int)

data Direction = North | South | East | West deriving (Show)
data Action = F Int | N Int | S Int | E Int | W Int | L Int | R Int

parse :: String -> Action
parse (ch:rest) = case ch of
  'F' -> F val
  'L' -> L val
  'R' -> R val
  'N' -> N val
  'S' -> S val
  'E' -> E val
  'W' -> W val
  where val = read rest

move :: V2 -> Direction -> Int -> V2
move (x,y) North magnitude = (x, y - magnitude)
move (x,y) South magnitude = (x, y + magnitude)
move (x,y) East  magnitude = (x + magnitude, y)
move (x,y) West  magnitude = (x - magnitude, y)

turn :: Direction -> Action -> Direction
turn heading (L angle) = head $ drop (angle `div` 90) $ iterate turnLeft heading
  where turnLeft North = West
        turnLeft South = East
        turnLeft East  = North
        turnLeft West  = South
turn heading (R angle) = head $ drop (angle `div` 90) $ iterate turnRight heading
  where turnRight North = East
        turnRight South = West
        turnRight East  = South
        turnRight West  = North

rotate :: V2 -> Action -> V2
rotate position (L angle) = head $ drop (angle `div` 90) $ iterate rotateLeft position
  where rotateLeft (x,y) = (y, x * (-1))
rotate position (R angle) = head $ drop (angle `div` 90) $ iterate rotateRight position
  where rotateRight (x,y) = (y * (-1), x)

solve12a :: [Action] -> Int
solve12a actions = let (_, finalPosition) = foldl step (East, (0,0)) actions in
                     manhattanDistance finalPosition (0, 0)
  where step :: (Direction, V2) -> Action -> (Direction, V2)
        step (heading, position) action
          = case action of
              F value -> (heading, move position heading value)
              L _     -> (turn heading action, position)
              R _     -> (turn heading action, position)
              N value -> (heading, move position North value)
              S value -> (heading, move position South value)
              E value -> (heading, move position East  value)
              W value -> (heading, move position West  value)

solve12b :: [Action] -> Int
solve12b actions = let (_, finalPosition, _) = foldl step (East, (0, 0), (10, -1)) actions in
                     manhattanDistance finalPosition (0, 0)
  where step (heading, position, waypoint) action
          = case action of
              F value -> let intermediatePosition = move position East ((fst waypoint) * value) in
                           (heading, move intermediatePosition North ((snd waypoint) * value), waypoint)
              L _     -> (heading, position, rotate waypoint action)
              R _     -> (heading, position, rotate waypoint action)
              N value -> (heading, position, move waypoint North value)
              S value -> (heading, position, move waypoint South value)
              E value -> (heading, position, move waypoint East value)
              W value -> (heading, position, move waypoint West value)

day12 :: [String] -> IO ()
day12 input = do
  let commands = map parse input
  print $ solve12a commands
  print $ solve12b commands
