module IntCode (runProgram, programResult, day02) where

import qualified Data.Sequence as S

type Program = [Int]
type Memory = S.Seq Int

execute :: Int -> (Int -> Int -> Int) -> Memory -> Memory
execute i op m =
  let operand1 = m `S.index` (m `S.index` (i + 1))
      operand2 = m `S.index` (m `S.index` (i + 2))
  in S.update (m `S.index` (i + 3)) (op operand1 operand2) m

runProgram :: Int -> Memory -> Memory
runProgram i m =
  case m `S.index` i of
    1  -> runProgram (i + 4) $ execute i (+) m
    2  -> runProgram (i + 4) $ execute i (*) m
    99 -> m
    _  -> error "world gone mad"

programResult :: Program -> Int
programResult program = (runProgram 0 (S.fromList program)) `S.index` 0

program :: (Int, Int) -> Program -> Program
program (noun, verb) input = take 1 input ++ [noun, verb] ++ drop 3 input

solve1 :: Program -> Int
solve1 = programResult . program (12, 2)

solve2 :: Program -> Int
solve2 input = head [ (100 * noun) + verb | noun <- [0..99]
                                          , verb <- [0..99]
                                          , (programResult (program (noun, verb) input)) == 19690720
                                          ]

day02 :: [Int] -> IO ()
day02 input = do
  print $ solve1 input
  print $ solve2 input

