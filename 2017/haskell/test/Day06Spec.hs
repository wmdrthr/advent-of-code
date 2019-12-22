module Main where

import Test.Hspec
import Day06 as Day06

spec :: Spec
spec = do
  describe "Day 6" $ do
    context "solve" $ do

      it "returns (5, 4) for [0, 2, 7, 0]" $ do
        Day06.solve [0, 2, 7, 0] `shouldBe` (5, 4)

main :: IO ()
main = hspec spec
