module SyntaxScoringSpec (spec) where

import Test.Hspec
import SyntaxScoring as SUT

spec :: Spec
spec = do
  let testInput = ["[({(<(())[]>[[{[]{<()<>>",
                   "[(()[<>])]({[<{<<[]>>(",
                   "{([(<{}[<>[]}>{[]{[(<()>",
                   "(((({<>}<{<{<>}{[]{[]{}",
                   "[[<[([]))<([[{}[[()]]]",
                   "[{[{({}]{}}([{[{{{}}([]",
                   "{<[[]]>}<{[{[{[]{()[[[]",
                   "[<(<(<(<{}))><([]([]()",
                   "<{([([[(<>()){}]>(<<{{",
                   "<{([{{}}[<[[[<>{}]]]>[]]"]
  let testScores = map SUT.scoreLine testInput
  it "Part 1: corrupted => 26397" $ do SUT.solve1 testScores `shouldBe` 26397
  it "Part 2: invalid => 288957" $ do SUT.solve2 testScores `shouldBe` 288957
