module Main where

import Test.Hspec
import Day3 as Day3

spec :: Spec
spec = do
  describe "Day 3" $ do
    context "solve1" $ do

      it "returns 0 for 1" $ do
        Day3.solve1 1 `shouldBe` 0

      it "returns 3 for 12" $ do
        Day3.solve1 12 `shouldBe` 3

      it "returns 2 for 23" $ do
        Day3.solve1 23 `shouldBe` 2

      it "returns 31 for 1024" $ do
        Day3.solve1 1024 `shouldBe` 31



main :: IO ()
main = hspec spec
