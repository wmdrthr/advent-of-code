{-# LANGUAGE ViewPatterns  #-}

module Day7 (
  day7,
  solve1,
  solve2
  ) where

import Data.Char
import Data.Tree
import Control.Applicative ((<|>))
import Data.List           (sortOn)
import Data.Foldable       (toList)
import Data.Maybe          (mapMaybe, listToMaybe, fromJust)
import qualified Data.Set as S
import qualified Data.Map as M


parseLine :: String -> (String, (Int, S.Set String))
parseLine (words->n:w:ls) =
  (n, (read w, S.fromList (filter isAlpha <$> drop 1 ls)))
parseLine _ = error "Error parsing line"

findRoot :: [String] -> String
findRoot input = S.findMax $ M.keysSet m `S.difference` allChildren
  where m = M.fromList (map parseLine input)
        allChildren = S.unions (snd <$> toList m)

buildTree :: M.Map String (Int, S.Set String) -> String -> Tree Int
buildTree nodes root = unfoldTree getChildren root
  where getChildren n = let (w, cs) = nodes M.! n
                        in (w, toList cs)

weightAdjustment :: Tree Int -> Maybe Int
weightAdjustment tree = listToMaybe badChildren <|> anomaly
  where badChildren = mapMaybe weightAdjustment $ subForest tree
        anomaly = case sortOn (length . snd) (M.toList weightMap) of
                    []                   -> Nothing
                    [_]                  -> Nothing
                    [(w1, [w]), (w2, _)] -> Just (w + (w2 - w1))
                    _                    -> error "found more than one bad weight"
        weightMap = M.fromListWith (++) . map (\t -> (sum t, [rootLabel t])) $ subForest tree


solve1 :: [String] -> String
solve1 = findRoot

solve2 :: [String] -> Int
solve2 input = case (weightAdjustment tree) of
                 Just w -> w
                 Nothing -> 0
  where root = findRoot input
        tree = buildTree (M.fromList $ map parseLine input) root

day7 :: [String] -> IO ()
day7 input = do
  print $ solve1 input
  print $ solve2 input
