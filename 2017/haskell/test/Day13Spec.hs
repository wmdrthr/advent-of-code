module Main where

import Test.Hspec
import Day13 as Day13

testInput = ["0: 3",
             "1: 2",
             "4: 4",
             "6: 4"]

spec :: Spec
spec = do

  describe "Day 13" $ do

    context "solve1" $ do

      it "returns 24 for the test input" $ do
        Day13.solve1 testInput `shouldBe` 24

    context "solve2" $ do

      it "returns 10 for the test input" $ do
        Day13.solve2 testInput `shouldBe` 10

main :: IO ()
main = hspec spec
