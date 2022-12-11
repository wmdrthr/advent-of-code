module RockPaperScissors where

data Move = Rock | Paper | Scissors deriving (Show, Eq)
data Outcome = Win | Draw | Loss deriving (Show, Eq)

shapeScore :: Move -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

outcomeScore :: Outcome -> Int
outcomeScore Win  = 6
outcomeScore Draw = 3
outcomeScore Loss = 0

parseOpponentMove :: String -> Move
parseOpponentMove turn = parse (head turn)
  where parse 'A' = Rock
        parse 'B' = Paper
        parse 'C' = Scissors
        parse  _  = error "invalid turn"

parsePlayerMove :: String -> Move
parsePlayerMove turn = parse (last turn)
  where parse 'X' = Rock
        parse 'Y' = Paper
        parse 'Z' = Scissors
        parse  _  = error "invalid turn"

parseDesiredOutcome :: String -> Outcome
parseDesiredOutcome turn = parse (last turn)
  where parse 'X' = Loss
        parse 'Y' = Draw
        parse 'Z' = Win
        parse  _  = error "invalid turn"

match :: Move -> Move -> Outcome
match Rock     Rock     = Draw
match Rock     Paper    = Win
match Rock     Scissors = Loss
match Paper    Rock     = Loss
match Paper    Paper    = Draw
match Paper    Scissors = Win
match Scissors Rock     = Win
match Scissors Paper    = Loss
match Scissors Scissors = Draw

play :: Move -> Outcome -> Move
play move Draw = move
play Rock     Win  = Paper
play Rock     Loss = Scissors
play Paper    Win  = Scissors
play Paper    Loss = Rock
play Scissors Win  = Rock
play Scissors Loss = Paper

turnScore :: String -> Int
turnScore turn = shapeScore playerMove + outcomeScore (match opponentMove playerMove)
  where playerMove   = parsePlayerMove turn
        opponentMove = parseOpponentMove turn

playScore :: String -> Int
playScore turn = shapeScore (play opponentMove desiredOutcome) + outcomeScore desiredOutcome
  where playerMove     = parsePlayerMove turn
        opponentMove   = parseOpponentMove turn
        desiredOutcome = parseDesiredOutcome turn

solve1 :: [String] -> Int
solve1 = sum . map turnScore

solve2 :: [String] -> Int
solve2 = sum . map playScore

day02 :: [String] -> IO ()
day02 input = do
  print $ solve1 input
  print $ solve2 input
