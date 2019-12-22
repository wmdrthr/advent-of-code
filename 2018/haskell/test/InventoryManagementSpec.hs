module InventoryManagementSpec (spec) where

import Test.Hspec
import InventoryManagement

spec :: Spec
spec = do
  context "part 1" $ do
    it "[abcdef,bababc,abbcde,abcccd,aabcdd,abcdee,ababab] => 12" $ do
      InventoryManagement.solve1 ["abcdef"
                                 ,"bababc"
                                 ,"abbcde"
                                 ,"abcccd"
                                 ,"aabcdd"
                                 ,"abcdee"
                                 ,"ababab"] `shouldBe` 12
  context "part 2" $ do
    it "[abcde,fghij,klmno,pqrst,fguij,axcye,wvxyz] => fgij" $ do
      InventoryManagement.solve2 ["abcde"
                                 ,"fghij"
                                 ,"klmno"
                                 ,"pqrst"
                                 ,"fguij"
                                 ,"axcye"
                                 ,"wvxyz"] `shouldBe` "fgij"

