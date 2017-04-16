-- You can benchmark your code quickly and effectively with Criterion.
-- See its website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import WaveToy1

default (Int)

main :: IO ()
main = defaultMain [
  -- bgroup "Grid" [bench "init" $ nf (initGrid (0.0::Double) (0.0, 1.0)) 1001]
  bgroup "Grid" [bench "init" $
                 whnf (\np -> let g = initGrid (0.0::Double) (0.0, 1.0) np
                              in energyGrid g) 1001]
  ]
