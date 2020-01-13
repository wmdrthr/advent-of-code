module IntCodeSpec (spec) where

import Test.Hspec
import IntCode as SUT

import qualified Data.Sequence as S
import           Data.Foldable    (toList)

spec :: Spec
spec = do
  context "Part 1" $ do
    it "[1,0,0,0,99] -> [2,0,0,0,99]" $ do
      let result = SUT.runProgram 0 (S.fromList [1,0,0,0,99])
      toList result `shouldBe` [2,0,0,0,99]
    it "[2,3,0,3,99] -> [2,3,0,6,99]" $ do
      let result = SUT.runProgram 0 (S.fromList [2,3,0,3,99])
      toList result `shouldBe` [2,3,0,6,99]
    it "[2,4,4,5,99,0] -> [2,4,4,5,99,9801]" $ do
      let result = SUT.runProgram 0 (S.fromList [2,4,4,5,99,0])
      toList result `shouldBe` [2,4,4,5,99,9801]
    it "[1,1,1,4,99,5,6,0,99] -> [30,1,1,4,2,5,6,0,99]" $ do
      let result = SUT.runProgram 0 (S.fromList [1,1,1,4,99,5,6,0,99])
      toList result `shouldBe` [30,1,1,4,2,5,6,0,99]
    it "[1,9,10,3,2,3,11,0,99,30,40,50] -> 3500" $ do
      programResult [1,9,10,3,2,3,11,0,99,30,40,50] `shouldBe` 3500



