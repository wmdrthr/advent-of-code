module Main where

import Test.Hspec
import Day9 as Day9

spec :: Spec
spec = do
  describe "Day 9" $ do
    context "solve1" $ do

      it "returns 1 for {}" $ do
        Day9.solve1 "{}" `shouldBe` 1

      it "returns {{{}}}" $ do
        Day9.solve1 "{{{}}}" `shouldBe` 6

      it "returns 5 for {{},{}}" $ do
        Day9.solve1 "{{},{}}" `shouldBe` 5

      it "returns 16 for {{{},{},{{}}}}" $ do
        Day9.solve1 "{{{},{},{{}}}}" `shouldBe` 16

      it "returns 1 for {<a>,<a>,<a>,<a>}" $ do
        Day9.solve1 "{<a>,<a>,<a>,<a>}" `shouldBe` 1

      it "returns 9 for {{<ab>},{<ab>},{<ab>},{<ab>}}" $ do
        Day9.solve1 "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9

      it "returns 9 for {{<!!>},{<!!>},{<!!>},{<!!>}}" $ do
        Day9.solve1 "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9

      it "returns 3 for {{<a!>},{<a!>},{<a!>},{<ab>}}" $ do
        Day9.solve1 "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3

    context "solve2" $ do

      it "returns 0 for <>" $ do
        Day9.solve2 "<>" `shouldBe` 0

      it "returns 17 for <random characters>" $ do
        Day9.solve2 "<random characters>"  `shouldBe` 17

      it "returns 3 for <<<<>" $ do
        Day9.solve2 "<<<<>" `shouldBe` 3

      it "returns 2 for <{!>}>" $ do
        Day9.solve2 "<{!>}>" `shouldBe` 2

      it "returns 0 for <!!>" $ do
        Day9.solve2 "<!!>" `shouldBe` 0

      it "returns 0 for <!!!>>" $ do
        Day9.solve2 "<!!!>>" `shouldBe` 0

      it "returns 10 for <{o\"i!a,<{i<a>" $ do
        Day9.solve2 "<{o\"i!a,<{i<a>" `shouldBe` 10


main :: IO ()
main = hspec spec
