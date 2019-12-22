module Main where

import Test.Hspec
import Day04 as Day04

spec :: Spec
spec = do
  describe "Day 4" $ do
    context "valid1" $ do

      it "returns True for 'aa bb cc dd ee'" $ do
        Day04.valid1 "aa bb cc dd ee" `shouldBe` True

      it "returns False for 'aa bb cc dd aa'" $ do
        Day04.valid1 "aa bb cc dd aa" `shouldBe` False

      it "returns True for 'aa bb cc dd aaa'" $ do
        Day04.valid1 "aa bb cc dd aaa" `shouldBe` True

    context "valid2" $ do

      it "returns True for 'abcde fghij'" $ do
        Day04.valid2 "abcde fghij" `shouldBe` True

      it "returns False for 'abcde xyz ecdab'" $ do
        Day04.valid2 "abcde xyz ecdab" `shouldBe` False

      it "returns True for 'a ab abc abd abf abj'" $ do
        Day04.valid2 "a ab abc abd abf abj" `shouldBe` True

      it "returns True for 'iiii oiii ooii oooi oooo'" $ do
        Day04.valid2 "iiii oiii ooii oooi oooo" `shouldBe` True

      it "returns False for 'oiii ioii iioi iiio'" $ do
        Day04.valid2 "oiii ioii iioi iiio" `shouldBe` False


main :: IO ()
main = hspec spec
