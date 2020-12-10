module AdapterArraySpec where

import Test.Hspec
import AdapterArray as SUT
import Data.Set     as S

testInputs = [ [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4],
               [28, 33, 18, 42, 31, 14, 46, 20, 48, 47,
                24, 23, 49, 45, 19, 38, 39, 11, 1, 32,
                25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
             ]


spec :: Spec
spec = do

  context "Test Case 1" $ do
    let ratings = S.fromList (testInputs !! 0)
    let maxRating = maximum (testInputs !! 0)
    it "Differences => 35" $ do SUT.differences ratings maxRating `shouldBe` 35
    it "Number of Chains > 8" $ do SUT.countAdapterChains ratings maxRating `shouldBe` 8

  context "Test Case 2" $ do
    let ratings = S.fromList (testInputs !! 1)
    let maxRating = maximum (testInputs !! 1)
    it "Differences => 220" $ do SUT.differences ratings maxRating `shouldBe` 220
    it "Number of Chains > 19208" $ do SUT.countAdapterChains ratings maxRating `shouldBe` 19208
