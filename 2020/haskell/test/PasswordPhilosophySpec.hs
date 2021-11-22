module PasswordPhilosophySpec (spec) where

import Test.Hspec
import PasswordPhilosophy as SUT

spec :: Spec
spec = do

  context "Part 1" $ do
    it "1-3 a: abcde => True" $ do SUT.valid1 ("abcde", 'a', 1, 3) `shouldBe` True
    it "1-3 b: cdefg => False" $ do SUT.valid1 ("cdefg", 'b', 1, 3) `shouldBe` False
    it "2-9 c: ccccccccc => True" $ do SUT.valid1 ("ccccccccc", 'c', 2, 9) `shouldBe` True

  context "Part 2" $ do
    it "1-3 a: abcde => True" $ do SUT.valid2 ("abcde", 'a', 1, 3) `shouldBe` True
    it "1-3 b: cdefg => False" $ do SUT.valid2 ("cdefg", 'b', 1, 3) `shouldBe` False
    it "2-9 c: ccccccccc => False" $ do SUT.valid2 ("ccccccccc", 'c', 2, 9) `shouldBe` False
