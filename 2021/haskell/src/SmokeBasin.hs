
{-# LANGUAGE NamedFieldPuns  #-}

module SmokeBasin where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char    (ord)
import Data.Ord     (comparing)
import Data.List    (sort)
import AdventOfCode


lowPoint :: Grid Int -> V2 -> Bool
lowPoint heightMap@Grid{points} point = all lower (neighbors heightMap point)
  where lower other = (height point) < (height other)
        height p    = points M.! p

solve1 :: Grid Int -> ([V2], Int)
solve1 heightMap@Grid{points} = (lowPoints, riskLevel)
  where lowPoints = filter (lowPoint heightMap) (M.keys points)
        riskLevel = sum $ map (\p -> 1 + (points M.! p)) lowPoints

highNeighbors :: Grid Int -> V2 -> [V2]
highNeighbors heightMap point = filter higher (neighbors heightMap point)
  where higher p = (height p) /= 9 && (height p) > (height point)
        height p = (points heightMap) M.! p

basin :: Grid Int -> V2 -> [V2]
basin heightMap start = search (S.fromList [start]) [start]
  where search history [] = S.toList history
        search history (current:xs) =
          let newPoints = filter (\p -> not $ S.member p history) (highNeighbors heightMap current)  in
            search (S.union history (S.fromList newPoints)) (xs ++ newPoints)

solve2 :: Grid Int -> [V2] -> Int
solve2 heightMap lowPoints = product $ take 3 $ reverse $ sort $ map length $ map (basin heightMap) lowPoints

parse :: Char -> Int
parse v = (ord v) - (ord '0')

day09 :: [String] -> IO ()
day09 input = do
  let heightMap = parseGrid parse input
  let (lowPoints, riskLevel) = solve1 heightMap
  print $ riskLevel
  print $ solve2 heightMap lowPoints
