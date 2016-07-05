{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict         #-}
{-# LANGUAGE StrictData     #-}

-- | Criterionized version of Bench1.

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
import           Types                       (for_, forkJoin, rand)

import qualified Data.Concurrent.Compact.Adaptive.PureToCompact as PCM
import qualified Data.Concurrent.PureMap                        as PM

data Env =
       Env
         { pm  :: PM.PureMap Int64 Int64
         , pcm :: PCM.AdaptiveMap Int64 Int64
         }
  deriving (Generic, NFData)

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

seed :: Word64
seed = 4096

range :: Int64
range = 2 ^ 10

{-# NOINLINE size #-}
size :: Int64
size = unsafePerformIO $ read <$> getEnv "SIZE"

setupPure :: IO (PM.PureMap Int64 Int64)
setupPure = do
  pm <- PM.newMap
  _ <- forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    for_ 0 size $ \v -> do
      k <- rand g range
      PM.ins k v pm
  return pm

setupCNFPure :: IO (PCM.AdaptiveMap Int64 Int64)
setupCNFPure = do
  pcm <- PCM.newMap
  _ <- forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    for_ 0 size $ \v -> do
      k <- rand g range
      PCM.ins k v pcm
  return pcm

setupEnv :: IO Env
setupEnv = do
  pm <- setupPure
  pcm <- setupCNFPure
  PCM.transition pcm
  return $! Env pm pcm

nopBench :: Env -> Benchmarkable
nopBench _ = Benchmarkable $
  \n -> void . forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    for_ 0 (n `quot` fromIntegral threads) $ \_ -> do
      _ <- rand g range
      return ()

pureBench :: Env -> Benchmarkable
pureBench Env { pm } = Benchmarkable $
  \n -> void . forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    for_ 0 (n `quot` fromIntegral threads) $ \_ -> do
      ix <- rand g range
      PM.get ix pm

cnfpureBench :: Env -> Benchmarkable
cnfpureBench Env { pcm } = Benchmarkable $
  \n -> void . forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    for_ 0 (n `quot` fromIntegral threads) $ \_ -> do
      ix <- rand g range
      PCM.get ix pcm

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~env ->
        bgroup "Bench3"
        [ bench "NOP" $ nopBench env
        , bench "Pure" $ pureBench env
        , bench "CNFPure" $ cnfpureBench env
        ]
    ]
