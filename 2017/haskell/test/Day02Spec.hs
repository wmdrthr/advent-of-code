module Main where

import Test.Hspec
import Day02 as Day02

spec :: Spec
spec = do
  describe "Day 2" $ do
    context "solve1" $ do

      it "returns 18 for the sample input" $ do
        Day02.solve1 [[5, 1, 9, 5],
                     [7, 5, 3],
                     [2, 4, 6, 8]] `shouldBe` 18

    context "solve2" $ do

      it "returns 9 for the sample input" $ do
        Day02.solve2 [[5, 9, 2, 8],
                     [9, 4, 7, 3],
                     [3, 8, 6, 5]] `shouldBe` 9


main :: IO ()
main = hspec spec
