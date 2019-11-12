module Main where

import Test.Hspec
import Day10 as Day10

spec :: Spec
spec = do
  describe "Day 10" $ do
    context "solve1" $ do

      it "returns the expected output for the test input" $ do
        Day10.solve1 "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110" `shouldBe` 23874

    context "solve2" $ do

      it "returns 'a2582a3a0e66e6e86e3812dcb672a272' for the empty string" $ do
        Day10.solve2 "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"

      it "returns '33efeb34ea91902bb2f59c9920caa6cd' for the input 'AoC 2017'" $ do
        Day10.solve2 "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"

      it "returns '3efbe78a8d82f29979031a4aa0b16a9d' for the input '1,2,3'" $ do
        Day10.solve2 "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"

      it "returns '63960835bcdc130f0b66d7ff4f6a5a8e' for the input '1,2,4'" $ do
        Day10.solve2 "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"

      it "returns the expected output for the test input" $ do
        Day10.solve2 "225,171,131,2,35,5,0,13,1,246,54,97,255,98,254,110" `shouldBe` "e1a65bfb5a5ce396025fab5528c25a87"

main :: IO ()
main = hspec spec
