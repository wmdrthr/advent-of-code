module CrossedWires (solve1, solve2, day03) where

import           Prelude         hiding (Left, Right)
import           AdventOfCode
import           Data.List.Split (splitOn)
import qualified Data.Map        as M
import qualified Data.Set        as S


type Vector = (Int, Int)

wirePath :: String -> [Vector]
wirePath = concatMap parse . splitOn ","
  where parse (dir:dist) = replicate (read dist) $
                           case dir of
                             'U' -> (0, (-1))
                             'D' -> (0, 1)
                             'L' -> ((-1), 0)
                             'R' -> (1, 0)

visited :: [Vector] -> S.Set Vector
visited = S.fromList . drop 1 . scanl go (0,0)
  where go (px,py) (vx,vy) = (px+vx,py+vy)

signalDelay :: [Vector] -> M.Map Vector Int
signalDelay =  M.fromListWith min . drop 1 . flip zip [0..] . scanl go (0,0)
  where go (px,py) (vx,vy) = (px+vx,py+vy)

solve1 :: [String] -> Int
solve1 wires = minimum (S.map (manhattanDistance (0,0)) (S.intersection w1 w2))
  where [w1,w2] = map (visited . wirePath) wires

solve2 :: [String] -> Int
solve2 wires = minimum (M.intersectionWith (+) w1 w2)
  where [w1, w2] = map (signalDelay . wirePath) wires

day03 :: [String] -> IO ()
day03 input = do
  print $ solve1 input
  print $ solve2 input
