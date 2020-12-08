module HandheldHalting where

import qualified Data.Set as S

data Instr = Acc Int | Jmp Int | Nop Int
  deriving Show

parse :: String -> Instr
parse line = case words line of
               ["acc", n] -> Acc $ num n
               ["jmp", n] -> Jmp $ num n
               ["nop", n] -> Nop $ num n
  where num = read . dropWhile (=='+')

execute :: [Instr] -> (Bool, Int)
execute program = go 0 S.empty 0
  where go ip history acc
          | ip `S.member` history = (False, acc)
          | ip >= (length program) = (True, acc)
          | otherwise             = case program !! ip of
                                      Acc n -> go (succ ip) (S.insert ip history) (acc + n)
                                      Jmp n -> go (ip +  n) (S.insert ip history) acc
                                      Nop _ -> go (succ ip) (S.insert ip history) acc

replace :: [Instr] -> Int -> [Instr]
replace program n = map replace' (zip [0..] program)
  where replace' (idx,instr)
          | idx /= n  = instr
          | otherwise = case instr of
                          Acc n -> Acc n
                          Jmp n -> Nop n
                          Nop n -> Jmp n

replaceAndExecute :: [Instr] -> Int -> (Bool, Int)
replaceAndExecute program n = if check (program !! n)
                              then execute $ replace program n
                              else (False, 0)
  where check (Acc _) = False
        check _       = True

solve8a :: [Instr] -> Int
solve8a = snd . execute

solve8b :: [Instr] -> Int
solve8b program = solve program [0..]
  where solve program ns = let (halting, res) = replaceAndExecute program (head ns) in
                                 if halting then res else solve program (tail ns)

day08 :: [String] ->  IO ()
day08 input = do
  let program = map parse input
  print $ solve8a program
  print $ solve8b program
