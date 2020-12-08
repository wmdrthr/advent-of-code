module HandyHaversacks where

import           AdventOfCode
import qualified Data.Map        as M
import           Data.List.Split (splitOn)

type Rules = M.Map String (M.Map String Int)

bagCheck :: Rules -> String -> Bool
bagCheck rules bagColor = let innerRules = rules M.! bagColor in
                            if M.member "shiny gold" innerRules
                            then True
                            else any (bagCheck rules) (M.keys innerRules)

bagCount :: Rules -> String -> Int
bagCount rules bagColor = let innerRules = rules M.! bagColor in
                            M.foldlWithKey accumulate 0 innerRules
  where accumulate count k v = count + v + (v * (bagCount rules k))

parseInnerRule :: String -> (String, Int)
parseInnerRule rule = let (count:colorA:colorB:_) = splitOn " " rule in
                        ((colorA ++ " " ++ colorB), read count)

parseRules :: Rules -> String -> Rules
parseRules rules line
  | children == "no other bags." = M.insert parentColor M.empty rules
  | otherwise                    = M.insert parentColor (M.fromList innerRules) rules
  where (parent:children:_) = splitOn " contain " line
        parentColor = substring 0 ((length parent) - 5) parent
        innerRules = map (parseInnerRule . trim) (splitOn "," children)

solve7a :: Rules -> Int
solve7a rules = length $ filter (bagCheck rules) (M.keys rules)

solve7b :: Rules -> Int
solve7b rules = bagCount rules "shiny gold"

day07 :: [String] -> IO ()
day07 input = do
  let rules = foldl parseRules M.empty input
  print $ solve7a rules
  print $ solve7b rules
