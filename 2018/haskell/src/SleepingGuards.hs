module SleepingGuards (
  sleepingGuards,
  solve1,
  solve2,
  prepareInput
  ) where

import AdventOfCode (cardinality, maxKey)

import Data.Time (UTCTime, readSTime, defaultTimeLocale, LocalTime, todMin, localTimeOfDay)
import Data.List (sort)

type Guard = Int
data Action = Start Guard | Wake | Sleep
  deriving (Show, Read, Eq, Ord)

parseAction :: String -> Action
parseAction str = case words str of
  ["wakes", "up"]     -> Wake
  ["falls", "asleep"] -> Sleep
  ["Guard", '#':guardid, "begins", "shift"] -> Start (read guardid)
  _                   -> error ("error parsing action: " ++ str)

parseLine :: String -> (LocalTime, Action)
parseLine str = case readSTime True defaultTimeLocale "[%Y-%m-%d %H:%M] " str of
  [(time, remaining)] -> (time, parseAction remaining)
  _                   -> error ("error parsing line: " ++ str)

toSleepMinutes :: [(LocalTime, Action)] -> [(Guard, Int)]
toSleepMinutes = clean . expand . go (error "no start")
  where
    clean     = filter (\(guardid, _) -> guardid > 0)
    getMinute = todMin . localTimeOfDay
    expand xs = [(guardid, i)
                | (guardid, (start, end)) <- xs
                , i <- [getMinute start .. getMinute end - 1]
                ]
    go _ ((_, Start guardid) : xs)                 = go guardid xs
    go guardid ((start, Sleep) : (end, Wake) : xs) = (guardid, (start, end)) : go guardid xs
    go _ []                                        = []
    go _ xs                                        = error "invalid sequence of events"

solve1 :: [(Guard, Int)] -> Int
solve1 sleepDetail = sleepiestGuard * maxHoursSlept
  where sleepiestGuard = maxKey (cardinality [n | (n, _) <- sleepDetail])
        maxHoursSlept  = maxKey (cardinality [m | (n,m) <- sleepDetail, n == sleepiestGuard])

solve2 :: [(Guard, Int)] -> Int
solve2 sleepDetail = sleepiestGuard * sleepiestHour
  where (sleepiestGuard, sleepiestHour) = maxKey (cardinality sleepDetail)

-- test helper
prepareInput :: [String] -> [(Guard, Int)]
prepareInput = toSleepMinutes . (map parseLine) . sort

sleepingGuards :: [String] -> IO ()
sleepingGuards input = do
  let sortedInput = sort input
  let sleepDetail = toSleepMinutes (map parseLine sortedInput)
  print $ solve1 sleepDetail
  print $ solve2 sleepDetail
