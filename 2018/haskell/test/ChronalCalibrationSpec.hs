module ChronalCalibrationSpec (spec) where

import Test.Hspec
import ChronalCalibration

spec :: Spec
spec = do
  context "part 1" $ do
      it "+1, -2, +3, +1 => 3" $ do ChronalCalibration.solve1 [1, -2, 3, 1] `shouldBe` 3
      it "+1, +1, +1 => 3"     $ do ChronalCalibration.solve1 [1, 1, 1]     `shouldBe` 3
      it "+1, +1, -2 => 0"     $ do ChronalCalibration.solve1 [1, 1, -2]    `shouldBe` 0
      it "-1, -2, -3 => -6"    $ do ChronalCalibration.solve1 [-1, -2, -3]  `shouldBe` -6
  context "part 2" $ do
      it "+1, -2, +3, +1 => 3"       $ do ChronalCalibration.solve2 [1, -2, 3, 1]      `shouldBe` 2
      it "+1, -1 => 0"               $ do ChronalCalibration.solve2 [1, -1]             `shouldBe` 0
      it "+3, +3, +4, -2, -4 => 10"  $ do ChronalCalibration.solve2 [3, 3, 4, -2, -4]  `shouldBe` 10
      it "-6, +3, +8, +5, -6 => 5"   $ do ChronalCalibration.solve2 [-6, 3, 8, 5, -6]  `shouldBe` 5
      it "+7, +7, -2, -7, -4 => 14"  $ do ChronalCalibration.solve2 [7, 7, -2, -7, -4] `shouldBe` 14
