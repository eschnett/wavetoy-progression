{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Monad.Loops

import WaveToy1

-- Parameters
tini = 0::Double
xmin = 0::Double
xmax = 1::Double
np = 20::Int
dx = (xmax - xmin) / fromIntegral np :: Double
alpha = 1/2 :: Double
dt = alpha * dx :: Double
niters = round (fromIntegral np / alpha) :: Int
out_every = niters `div` 10 :: Int

rhs s = rhsGrid (bcGrid s) s
inc g = g { iter = iter g + 1 }
step s = inc $ rk2Grid dt rhs s

main :: IO ()
main = do putStrLn "WaveToy1"
          let state = initGrid tini (xmin, xmax) np
          output state
          _ <- iterateUntilM
            (\s -> iter s >= niters)
            (\s -> do let s' = step s
                      output s'
                      return s')
            state
          return ()

output :: (Floating a, Show a) => Grid a (Cell a) -> IO ()
output state =
  if iter state == niters || iter state `mod` out_every == 0
  then do putStrLn $ "iteration: " ++ show (iter state)
          putStrLn $ "  time: " ++ show (time state)
          let energy = energyGrid state
          putStrLn $ "  energy: " ++ show energy
          let error_norm = normGrid $ errorGrid state
          putStrLn $ "  L2 error: " ++ show error_norm
  else do return ()
