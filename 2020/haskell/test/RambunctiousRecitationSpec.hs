module RambunctiousRecitationSpec where

import Test.Hspec
import RambunctiousRecitation as SUT

spec :: Spec
spec = do

  context "Test Inputs" $ do
    it "0,3,6 => 436" $ do SUT.memoryGame 2020 [0,3,6] `shouldBe` 436
    it "1,3,2 => 1" $ do SUT.memoryGame 2020 [1,3,2] `shouldBe` 1
    it "2,1,3 => 10" $ do SUT.memoryGame 2020 [2,1,3] `shouldBe` 10
    it "1,2,3 => 27" $ do SUT.memoryGame 2020 [1,2,3] `shouldBe` 27
    it "2,3,1 => 78" $ do SUT.memoryGame 2020 [2,3,1] `shouldBe` 78
    it "3,2,1 => 438" $ do SUT.memoryGame 2020 [3,2,1] `shouldBe` 438
    it "3,1,2 => 1836" $ do SUT.memoryGame 2020 [3,1,2] `shouldBe` 1836
