module EncodingErrorSpec where

import Test.Hspec
import EncodingError as SUT

testInput = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102,
             117, 150, 182, 127, 219, 299, 277, 309, 576]

spec :: Spec
spec = do

  context "Test Input" $ do
    it "target   => 127" $ do SUT.solve9a testInput 5   `shouldBe` 127
    it "weakness => 62"  $ do SUT.solve9b testInput 127 `shouldBe` 62
