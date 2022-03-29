module GiantSquid where

import Data.List       (maximumBy
                       ,minimumBy
                       ,transpose
                       ,splitAt)
import Data.Ord        (comparing)
import Data.List.Split (splitOn)

type Row = [(Int, Bool)]
type Board = [Row]

checkBoard :: Board -> Bool
checkBoard board = row || column
  where
    row    = any (all snd) board
    column = any (all snd) $ transpose board


markNumber :: Board -> Int -> Board
markNumber board number = map (map markCell) board
  where
    markCell (n, b) = if n == number then (n, True) else (n, b)

parseBoard :: [String] -> Board
parseBoard lines = map parseRow lines
  where parseRow line = map (\n -> (read n :: Int, False)) $ filter (/="") $ splitOn " " line

parseBoards :: [String] -> [Board]
parseBoards lines = parse' [] lines
  where
    parse' boards [] = boards
    parse' boards bl = let (l, rest) = splitAt 5 bl in
                               parse' (boards ++ [parseBoard l]) (drop 1 rest)

unmarkedSum :: Board -> Int
unmarkedSum board = sum . map fst . filter (not . snd) $ concat board

getWinningNumber :: Board -> [Int] -> (Int, Board)
getWinningNumber board = head . filter (checkBoard . snd) . zip [0 ..] . scanl markNumber board

solve1 :: [Board] -> [Int] -> Int
solve1 boards numbers = (numbers !! (idx - 1)) * (unmarkedSum winningBoard)
  where (idx, winningBoard) = minimumBy (comparing fst) $ map (`getWinningNumber` numbers) boards

solve2 :: [Board] -> [Int] -> Int
solve2 boards numbers = (numbers !! (idx - 1)) * (unmarkedSum winningBoard)
  where (idx, winningBoard) = maximumBy (comparing fst) $ map (`getWinningNumber` numbers) boards


day04 :: [String] -> IO ()
day04 input = do
  let calledNumbers = map read $ splitOn "," (head input) :: [Int]
  let boards = parseBoards (drop 2 input)
  print $ solve1 boards calledNumbers
  print $ solve2 boards calledNumbers
