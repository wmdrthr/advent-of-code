module PasswordsSpec (spec) where

import Test.Hspec
import Passwords as SUT

spec :: Spec
spec = do
  context "Part 1" $ do
    it "111111 -> valid"   $ do SUT.valid1 "111111" `shouldBe` True
    it "223450 -> invalid" $ do SUT.valid1 "223450" `shouldBe` False
    it "123789 -> invalid" $ do SUT.valid1 "123789" `shouldBe` False
  context "Part 2" $ do
    it "112233 -> valid"   $ do SUT.valid2 "112233" `shouldBe` True
    it "123444 -> invalid" $ do SUT.valid2 "123444" `shouldBe` False
    it "111122 -> valid"   $ do SUT.valid2 "111122" `shouldBe` True
