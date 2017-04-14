{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module WaveToy1 (Cell(..), initCell, errorCell, energyCell, rhsCell,
                 Grid(..), normGrid, initGrid, errorGrid, energyGrid, rhsGrid,
                 bcGrid, rk2Grid) where

import qualified Data.Vector as V
import Data.Monoid
import Control.Applicative

default (Int)



data Cell a = Cell { u, rho, vx :: a }
  deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance Applicative Cell where
  pure x = Cell x x x
  (Cell fu frho fvx) <*> (Cell u rho vx) = Cell (fu u) (frho rho) (fvx vx)

sineCell :: Floating a => (a, a) -> Cell a
sineCell (t, x) = Cell { u = cos (2*pi*t) * sin (2*pi*x),
                         rho = -2*pi * sin (2*pi*t) * sin (2*pi*x),
                         vx = 2*pi * cos (2*pi*t) * cos (2*pi*x) }

initCell :: Floating a => (a, a) -> Cell a
initCell = sineCell

errorCell :: Floating a => (a, a) -> Cell a -> Cell a
errorCell (t, x) cell = liftA2 (-) cell (sineCell (t, x))

energyCell :: Fractional a => Cell a -> a
energyCell (Cell u rho vx) = 1/2 * (rho^2 + vx^2)

rhsCell :: Fractional a => a -> (Cell a, Cell a) -> Cell a -> Cell a
rhsCell dx (lb, ub) c = Cell (rho c) (ddx vx) (ddx rho)
  where ddx f = (f ub - f lb) / (2 * dx)

-- reflecting boundaries: u -> -u
flipCell :: Num a => Cell a -> Cell a
flipCell (Cell u rho vx) = Cell (-u) (-rho) vx



data Grid b a = Grid { iter :: Int,
                       time :: b,
                       bnds :: (b, b),
                       cells :: V.Vector a }
  deriving (Read, Show)

instance Foldable (Grid b) where
  foldMap f g = foldMap f (cells g)

instance Functor (Grid b) where
  fmap f (Grid iter time bnds cells) = Grid iter time bnds $ fmap f cells

normGrid :: Floating a => Grid a (Cell a) -> a
normGrid g = sqrt (s2 / c)
  where s2 = getSum $ foldMap (foldMap (Sum . (^2))) (cells g)
        c = getSum $ foldMap (foldMap (Sum . const 1)) (cells g)

coordsVector :: Fractional a => (a, a) -> Int -> V.Vector a
coordsVector (xmin, xmax) np = V.generate np x
  where x i = xmin + dx * (fromIntegral i + 1/2)
        dx = (xmax - xmin) / fromIntegral np

initGrid :: Floating a => a -> (a, a) -> Int -> Grid a (Cell a)
initGrid time bnds np = Grid 0 time bnds $ V.map ini coords
  where ini x = initCell (time, x)
        coords = coordsVector bnds np

errorGrid :: Floating a => Grid a (Cell a) -> Grid a (Cell a)
errorGrid (Grid iter time bnds cells) =
  Grid iter time bnds $ V.zipWith err coords cells
  where err x cell = errorCell (time, x) cell
        coords = coordsVector bnds np
        np = V.length cells

energyGrid :: Fractional a => Grid a (Cell a) -> a
energyGrid (Grid _ _ (xmin, xmax) cells) =
  getSum (foldMap (Sum . energyCell) cells) * dx
  where dx = (xmax - xmin) / fromIntegral np
        np = V.length cells

rhsGrid :: Fractional a =>
           (Cell a, Cell a) -> Grid a (Cell a) -> Grid a (Cell a)
rhsGrid (lb, ub) (Grid iter time (xmin, xmax) cells) =
  Grid iter time (xmin, xmax) $ V.fromList rhs
  where rhs = rmin ++ rint ++ rmax
        rmin = [rhsCell dx (lb, cells V.! 1) (cells V.! 0)]
        rint = [rhsCell dx (cells V.! (i-1), cells V.! (i+1)) (cells V.! i) |
                i <- [1 .. np-2]]
        rmax = [rhsCell dx (cells V.! (np-2), ub) (cells V.! (np-1))]
        dx = (xmax - xmin) / fromIntegral np
        np = length cells

bcGrid :: Num a => Grid b (Cell a) -> (Cell a, Cell a)
bcGrid g = (flipCell cmin, flipCell cmax)
  where cmin = cells g V.! 0
        cmax = cells g V.! (np-1)
        np = length (cells g)

stepGrid :: (Fractional a, Applicative c) =>
            Grid a (c a) -> a -> Grid a (c a) -> Grid a (c a)
stepGrid (Grid it t bnds state) dt (Grid _ _ _ rhs) =
  Grid it (t + dt) bnds $ V.zipWith (liftA2 step) state rhs
  where step s r = s + dt * r

rk2Grid :: (Fractional a, Applicative c) =>
           a -> (Grid a (c a) -> Grid a (c a)) -> Grid a (c a) -> Grid a (c a)
rk2Grid dt rhs s0 =
  let r0 = rhs s0
      s1 = stepGrid s0 (dt/2) r0
      r1 = rhs s1
      s2 = stepGrid s0 dt r1
  in s2
