module CustomCustomsSpec where

import Test.Hspec
import CustomCustoms as SUT

customsForms =
  [ [ [ "abcx", "abcy", "abcz"] ],
    [ [ "abc" ] , [ "a", "b", "c" ] , [ "ab", "ac" ] , [ "a", "a", "a", "a"] , [ "b" ] ]
  ]


spec :: Spec
spec = do

  context "Part 1" $ do
    it "Test Case 1 => 6" $ do SUT.solve6a (head customsForms) `shouldBe` 6
    it "Test Case 2 => 11" $ do SUT.solve6a (last customsForms) `shouldBe` 11
  context "Part 2" $ do
    it "Test Case 1 => 3" $ do SUT.solve6b (head customsForms) `shouldBe` 3
    it "Test Case 2 => 6" $ do SUT.solve6b (last customsForms) `shouldBe` 6
