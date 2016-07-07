
-- | Benchmark the Ctrie freeze function.

module Main where

import           Control.DeepSeq
import           Control.Monad
import           Control.Exception
import           Criterion.Main
import           Criterion.Types
import           Data.Int
import           Data.Word
import           GHC.Generics
import           GHC.Conc
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG
import           Types                       (for_, forkJoin, rand, timeit)

import qualified Data.Vector as V
import qualified Data.Concurrent.Compact.Adaptive.PureToCompact as PCM
import qualified Data.Concurrent.Ctrie                          as CM
import           Data.IORef
import qualified Data.HashMap.Strict   as HM
import           Data.Hashable (Hashable)


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
  performGC; performGC
  return pm

type AllPerms = V.Vector CM.Perms

-- | Take a vector of perms and fork that many threads to do the
-- freeze.
parFreeze :: AllPerms -> CM.Map k v -> IO ()
parFreeze vec mp = do
  _ <- forkJoin (V.length vec) $ \ix -> do
         CM.freezeRandBottom (vec V.! ix) mp
  return ()

parFreezeConvert :: (Hashable k, Eq k) => 
                    AllPerms -> CM.Map k v -> IORef (HM.HashMap k v) -> IO ()
parFreezeConvert vec mp acc = do
  forkJoin (V.length vec) $ \ix -> do
     CM.freezeRandConvert (vec V.! ix) mp acc
  return ()

-- | Build the perms used by all threads.
mkAllPerms :: IO AllPerms
mkAllPerms = do 
  perms <- V.generateM threads (\_ -> CM.makePerms)
  evaluate (rnf perms)
  return perms


-- TODO: Criterionize.... but we need per-batch env initialization.
benchFreeze :: IO ()
benchFreeze = do
  perms <- mkAllPerms
  do mp  <- timeit "Fill map, first round" setupMap
     timeit "Freeze" (CM.freeze mp)

  do mp  <- timeit "" setupMap
     timeit "One FreezeRandBottom" (CM.freezeRandBottom (perms V.! 0) mp)
  
  do mp  <- timeit "" setupMap
     timeit "Par FreezeRandBottom" (parFreeze perms mp)
  putStrLn "Done."

benchConvert :: IO ()
benchConvert = 
  do perms <- mkAllPerms

     do mp  <- timeit "" setupMap
        timeit "Sequential freezeFold convert" $ void $
           CM.freezeFold (\h k v -> HM.insert k v h) HM.empty mp

     do mp  <- timeit "" setupMap
        acc <- newIORef HM.empty
        timeit "One-thread Freeze+Convert bottom-up" $ 
               CM.freezeRandConvert (perms V.! 0) mp acc

     do mp  <- timeit "" setupMap
        acc <- newIORef HM.empty
        timeit "Parallel Freeze+Convert bottom-up" $
               parFreezeConvert perms mp acc

     putStrLn "Done."


main :: IO ()
main = do setNumCapabilities threads
          putStrLn $ "Set numCap to "++show threads
          hFlush stdout
          -- benchFreeze
          hFlush stdout
          benchConvert
