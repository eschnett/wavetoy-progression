-- You can benchmark your code quickly and effectively with Criterion.
-- See its website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import qualified BenchWaveToy1
import qualified BenchWaveToy2

main :: IO ()
main = defaultMain $
  BenchWaveToy1.benchmarks ++
  BenchWaveToy2.benchmarks
