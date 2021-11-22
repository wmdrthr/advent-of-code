module BinaryBoardingSpec where

import Test.Hspec
import BinaryBoarding as SUT

spec :: Spec
spec = do

  context "Boarding Pass Seat IDs" $ do
    it "FBFBBFFRLR => 357" $ SUT.getSeatId "FBFBBFFRLR" `shouldBe` 357
    it "BFFFBBFRRR => 567" $ SUT.getSeatId "BFFFBBFRRR" `shouldBe` 567
    it "FFFBBBFRRR => 119" $ SUT.getSeatId "FFFBBBFRRR" `shouldBe` 119
    it "BBFFBBFRLL => 820" $ SUT.getSeatId "BBFFBBFRLL" `shouldBe` 820
