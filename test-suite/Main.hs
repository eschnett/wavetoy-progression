-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import WaveToy1

main :: IO ()
main = do
  test <- testSpec "wavetoy-progression" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  specCell
  specGrid

specCell :: Spec
specCell = parallel $ do
  describe "Cell" $ do
    describe "error" $ do
      it "is zero when exact" $ do
        let (t, x) = (0.1, 0.2)
            c = initCell (t, x) :: Cell Double
            e = errorCell (t, x) c
          in e `shouldBe` Cell 0 0 0
    describe "energy" $ do
      it "is zero for vacuum" $ do
        let c = Cell 1.0 0.0 0.0 :: Cell Double
            e = energyCell c
          in e `shouldBe` 0.0
      it "has the correct potential term" $ do
        let c = Cell 0.0 1.0 0.0 :: Cell Double
            e = energyCell c
          in e `shouldBe` 0.5
      it "has the correct kinetic term" $ do
        let c = Cell 0.0 0.0 1.0 :: Cell Double
            e = energyCell c
          in e `shouldBe` 0.5

specGrid :: Spec
specGrid = parallel $ do
  describe "Grid" $ do
    describe "init" $ do
      it "has the right time" $ do
        let g = initGrid 0.1 (0.0, 1.0) 10 :: Grid Double (Cell Double)
          in time g `shouldBe` 0.1
      it "has the right extent" $ do
        let g = initGrid 0.0 (0.1, 1.1) 10 :: Grid Double (Cell Double)
          in bnds g `shouldBe` (0.1, 1.1)
      it "has the right size" $ do
        let g = initGrid 0.0 (0.0, 1.0) 11 :: Grid Double (Cell Double)
          in length (cells g) `shouldBe` 11
