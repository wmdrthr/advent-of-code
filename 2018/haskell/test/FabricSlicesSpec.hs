module FabricSlicesSpec (spec) where

import Test.Hspec
import FabricSlices

testClaims = map FabricSlices.parseClaim ["#1 @ 1,3: 4x4"
                                         ,"#2 @ 3,1: 4x4"
                                         ,"#3 @ 5,5: 2x2"]

testFabric = FabricSlices.cutFabric $ testClaims

spec :: Spec
spec = do
  context "part 1" $ do
    it "#1 @ 1,3: 4x4, #2 @ 3,1: 4x4, #3 @ 5,5: 2x2 => 4" $ do
      FabricSlices.solve1 testFabric `shouldBe` 4
  context "part 2" $ do
    it "#1 @ 1,3: 4x4, #2 @ 3,1: 4x4, #3 @ 5,5: 2x2 => 3" $ do
      FabricSlices.solve2 testFabric testClaims `shouldBe` 3
