module Dive where

data Command = Forward | Down | Up deriving (Show, Eq)
type Move = (Command, Int)

parse :: String -> Move
parse line = (getCommand c, read v :: Int)
  where (c:v:[]) = words line
        getCommand "forward" = Forward
        getCommand "down" = Down
        getCommand "up" = Up


navigate :: (Int, Int) -> Move -> (Int, Int)
navigate (depth, position) (Forward, v) = (depth    , position + v)
navigate (depth, position) (Down   , v) = (depth + v, position    )
navigate (depth, position) (Up     , v) = (depth - v, position    )

navigateUsingAim :: (Int, Int, Int) -> Move -> (Int, Int, Int)
navigateUsingAim (aim, depth, position) (Forward, v) = (aim, depth + (aim * v), position + v)
navigateUsingAim (aim, depth, position) (Down   , v) = (aim + v, depth, position)
navigateUsingAim (aim, depth, position) (Up     , v) = (aim - v, depth, position)

solve1 :: [Move] -> Int
solve1 commands = depth * position
  where (depth, position) = foldl navigate (0, 0) commands

solve2 :: [Move] -> Int
solve2 commands = let (_, depth, position) = foldl navigateUsingAim (0, 0, 0) commands in
                    depth * position


day02 :: [String] -> IO ()
day02 input = do
  let commands = map parse input
  print $ solve1 commands
  print $ solve2 commands
