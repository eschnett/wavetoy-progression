{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import qualified TestWaveToy1
import qualified TestWaveToy2

main :: IO ()
main = do
  test <- testSpec "wavetoy-progression" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  TestWaveToy1.spec
  TestWaveToy2.spec
