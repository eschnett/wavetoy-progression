{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestWaveToy1 (spec) where

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Test.Tasty.QuickCheck

-- import Test.Tasty.SmallCheck

import WaveToy1

default (Int)

spec :: Spec
spec = parallel $ do
  describe "WaveToy1" $ do
    specCell
    specGrid

specCell :: Spec
specCell = parallel $ do
  describe "Cell" $ do
    describe "error" $ do
      it "is zero when exact" $ property $ do
        \(t, x) -> let c = initCell (t, x) :: Cell Double
                       e = errorCell (t, x) c
                   in e == Cell 0 0 0
    describe "energy" $ do
      it "is zero for vacuum" $ property $ do
        \u -> let c = Cell u 0.0 0.0 :: Cell Double
                  e = energyCell c
              in e == 0.0
      it "has the correct potential term" $ property $ do
        \rho -> let c = Cell 0.0 rho 0.0 :: Cell Double
                    e = energyCell c
                in e  == 0.5 * rho^2
      it "has the correct kinetic term" $ property $ do
        \vx -> let c = Cell 0.0 0.0 vx :: Cell Double
                   e = energyCell c
               in e == 0.5 * vx^2

specGrid :: Spec
specGrid = parallel $ do
  describe "Grid" $ do
    describe "init" $ do
      it "has the right time" $ property $ do
        \t (xmin, xmax) np ->
          np >= 0 && xmax > xmin ==>
          let g = initGrid t (xmin, xmax) np :: Grid Double (Cell Double)
          in time g == t
      it "has the right extent" $ property $ do
        \t (xmin, xmax) np ->
          np >= 0 && xmax > xmin ==>
          let g = initGrid t (xmin, xmax) np :: Grid Double (Cell Double)
          in bnds g == (xmin, xmax)
      it "has the right size" $ property $ do
        \t (xmin, xmax) np ->
          np >= 0 && xmax > xmin ==>
          let g = initGrid t (xmin, xmax) np :: Grid Double (Cell Double)
          in length (cells g) == np
