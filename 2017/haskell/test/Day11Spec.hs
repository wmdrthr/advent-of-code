module Main where

import Test.Hspec
import Day11 as Day11

spec :: Spec
spec = do
  describe "Day 11" $ do
    context "solve" $ do

      it "returns (3, 3) for 'ne,ne,ne'" $ do
        Day11.solve "ne,ne,ne" `shouldBe` (3, 3)

      it "returns (0, 2) for 'ne,ne,sw,sw'" $ do
        Day11.solve "ne,ne,sw,sw" `shouldBe` (0, 2)

      it "returns (2, 2) for 'ne,ne,s,s'" $ do
        Day11.solve "ne,ne,s,s" `shouldBe` (2, 2)

      it "returns (3, 3) for 'se,sw,se,sw,sw'" $ do
        Day11.solve "se,sw,se,sw,sw" `shouldBe` (3, 3)


main :: IO ()
main = hspec spec
