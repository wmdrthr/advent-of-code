module Main where

import Test.Hspec
import Day05 as Day05

spec :: Spec
spec = do
  describe "Day 5" $ do
    context "solve1" $ do

      it "returns 5 for [0, 3, 0, 1, -3]" $ do
        Day05.solve1 [0, 3, 0, 1, -3] `shouldBe` 5

    context "solve2" $ do

      it "returns 10 for [0, 3, 0, 1, -3]" $ do
        Day05.solve2 [0, 3, 0, 1, -3] `shouldBe` 10


main :: IO ()
main = hspec spec
