module RockPaperScissorsSpec (spec) where

import Test.Hspec
import RockPaperScissors (solve1, solve2)

spec :: Spec
spec = do
    let testInput = ["A Y", "B X", "C Z"]

    it "Part 1 (test input) => 15" $ do solve1 testInput `shouldBe` 15
    it "Part 2 (test input) => 12" $ do solve2 testInput `shouldBe` 12