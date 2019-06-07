module Main where

import Test.Hspec
import Day6 as Day6

spec :: Spec
spec = do
  describe "Day 6" $ do
    context "solve" $ do

      it "returns (5, 4) for [0, 2, 7, 0]" $ do
        Day6.solve [0, 2, 7, 0] `shouldBe` (5, 4)

main :: IO ()
main = hspec spec
