module ReportRepairSpec (spec) where

import Test.Hspec
import ReportRepair as SUT

spec :: Spec
spec = do
  let testInput = [1721, 979, 366, 299, 675, 1456]
  context "Part 1" $ do
    it "product of pairs => 514579" $ do SUT.solve 2 testInput `shouldBe` 514579
  context "Part 2" $ do
    it "product of triples => 241861950" $ do SUT.solve 3 testInput `shouldBe` 241861950
