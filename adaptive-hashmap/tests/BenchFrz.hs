
-- | Benchmark the Ctrie freeze function.

module Main where

import           Control.DeepSeq
import           Control.Monad
import           Criterion.Main
import           Criterion.Types
import           Data.Int
import           Data.Word
import           GHC.Generics
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure as PCG
import           Types                       (for_, forkJoin, rand, timeit)

import qualified Data.Concurrent.Compact.Adaptive.PureToCompact as PCM
import qualified Data.Concurrent.Ctrie                          as CM


{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

seed :: Word64
seed = 4096

range :: Int64
range = maxBound


{-# NOINLINE size #-}
size :: Int64
size = unsafePerformIO $ read <$> getEnv "SIZE"


-- This gets a reasonable parallel speedup with CTrie.
setupMap :: IO (CM.Map Int64 Int64)
setupMap = do
  pm <- CM.empty
  let sz0 = size `quot` fromIntegral threads
      sz1 = sz0 + (size `rem` fromIntegral threads)
  _ <- forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    let sz = if chunk == 0
             then sz1 :: Int64
             else sz0
    for_ 1 sz $ \v -> do
      k <- rand g range
      CM.insert k v pm
  return pm

main = do
  mp  <- timeit "Fill map" setupMap

  timeit "Freeze" (CM.freeze mp)

  putStrLn "Done."
