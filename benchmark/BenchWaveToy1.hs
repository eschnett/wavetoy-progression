module BenchWaveToy1 (benchmarks) where

-- You can benchmark your code quickly and effectively with Criterion.
-- See its website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import WaveToy1

default (Int)

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "WaveToy1"
    [ bgroup "Grid"
      [ bench "init" $
        whnf (\np -> let g = initGrid (0.0::Double) (0.0, 1.0) np
                     in energyGrid g) 1001
      , bench "rhs" $
        whnf (\np -> let g = initGrid (0.0::Double) (0.0, 1.0) np
                         bs = bcGrid g
                         g' = rhsGrid bs g
                     in energyGrid g') 1001
      , bench "rk2" $
        whnf (\np -> let g = initGrid (0.0::Double) (0.0, 1.0) np
                         rhs x = rhsGrid (bcGrid x) x
                         step = rk2Grid 1.0e-8 rhs
                         g' = iterate step g !! 10
                     in energyGrid g') 1001
      ]
    ]
  ]
