{-# LANGUAGE ViewPatterns #-}

module Day12 (
  day12,
  solve
  ) where

import AdventOfCode  (parseCommaSeparatedNumbers)
import Data.Graph    as G

parseLine :: String -> (String, Int, [Int])
parseLine (words -> node:_:rest) = (node
                                  , read node
                                  , parseCommaSeparatedNumbers $ unwords rest)

solve :: [String] -> (Int, Int)
solve input = ((length. head) comps, length comps)
  where comps     = components g
        (g, _, _) = G.graphFromEdges $ map parseLine input


day12 :: [String] -> IO ()
day12 input = do
  print $ solve input
