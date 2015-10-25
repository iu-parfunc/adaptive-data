{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main
       where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Extra
--import qualified Data.Atomics.Counter as C
import Data.Int
import Data.IORef
import System.Console.CmdArgs
import System.IO
import Data.Time.Clock
import GHC.Word
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified AdaptiveBag as AB
import qualified Data.Concurrent.PureBag as PB
import qualified Data.Concurrent.ScalableBag as SB

thread :: Int -> (Int64 -> IO()) -> (() -> IO(Maybe Int64)) -> Flag -> Barrier Bool -> IORef(Bool) -> Word64 -> IO(Int)
thread tid ws rs option bar cont seedn = do
  let loop i g = do
        c <- readIORef cont
        if c
          then do
          p <- PCG.uniformRD (0.0, 1.0) g :: IO Double
          a <- PCG.uniformI64 g :: IO Int64
          if p> ratio option
            then ws a
            else do
            !r <- rs ()
            return ()
          
          loop (i+1) g
          else return (i)

--  putStrLn $ "Thread " ++ show tid ++ "started"
  !b <- waitBarrier bar
--  putStrLn $ "Thread id: " ++ show tid ++  ", seed: " ++ show seedn
  if b
    then do
    g <- PCG.restore $ PCG.initFrozen seedn
    loop 0 g
    else return (0)

test :: Int -> (Int64 -> IO()) -> (() -> IO(Maybe Int64)) -> Flag -> PCG.GenIO -> IO (Double)
test threadn ws rs option gen = do
  !bar <- newBarrier
  !ref <- newIORef True
  !asyncs <- mapM (\tid -> do
                       s <- PCG.uniformW64 gen :: IO Word64
                       async $ thread tid ws rs option bar ref s) [1..threadn]
  signalBarrier bar True
  !start <- getCurrentTime
  threadDelay $ 1000 * duration option
  writeIORef ref False
  !end <- getCurrentTime
  !res <- mapM wait asyncs
  return $ ((fromIntegral $ sum res)::Double) / ((realToFrac $ diffUTCTime end start) * 1000.0 ::Double)

mean :: [Double] -> Double
mean xs = (sum xs) / ((realToFrac $ length xs) :: Double)

stddev :: [Double] -> Double
stddev xs = sqrt $ (sum (map (\x -> (x - avg) * (x - avg)) xs)) / len
  where len = (realToFrac $ length xs) :: Double
        avg = mean xs

run :: Int -> Flag -> IO()
run threadn option = do
  outh <- openFile ((file option) ++ "_" ++ (bench option) ++ ".csv") AppendMode
--  hPutStrLn outh "threadn,Mean,Stddev"
--  setStdGen $ mkStdGen $ seed option
  gen <- PCG.restore $ PCG.initFrozen $ seed option

  let loop i | i>0 = do
                 !ops <- case bench option of
                   "pure" -> do
                     !bag <- PB.newBag
                     test threadn (PB.add bag) (\() -> PB.remove bag) option gen
                   "scalable" -> do
                     !bag <- SB.newBag
                     test threadn (SB.add bag) (\() -> SB.remove bag) option gen
                   "adaptive" -> do
                     !bag <- AB.newBag
                     test threadn (AB.add bag) (\() -> AB.remove bag) option gen
                   _ -> test threadn (\_ -> do return ()) (\() -> return Nothing) option gen
                     
                 putStrLn $ show threadn ++ " threads: " ++ show ops ++ " ops/ms"
                 hFlush stdout
                 xs <- (loop $ i-1)
                 return $ ops : xs
      loop _ = return []

  xs <- loop $ (runs option) + (warmup option)
  let res = drop (warmup option) xs
  hPutStrLn outh $ (show threadn) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)

  hClose outh

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number: " ++ show threadn
  putStrLn $ "Duration:      " ++ show (duration option) ++ "ms"
  putStrLn $ "Ratio:         " ++ show (ratio option)
  putStrLn $ "Initial size:  " ++ show (initial option)
  putStrLn $ "Number of runs:" ++ show (runs option)
  putStrLn $ "Warmup runs:   " ++ show (warmup option)
  putStrLn $ "Seed:          " ++ show (seed option)
  putStrLn $ "File:          " ++ show (file option)

  if length (bench option) == 0
    then return ()
    else run threadn option
  return ()
  

data Flag
  = Flag {duration :: Int,
          ratio :: Double,
          initial :: Int,
          seed :: Word64,
          file :: String,
          runs :: Int,
          warmup :: Int,
          bench :: String}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {duration = 100 &= help "Duration",
             warmup = 5 &= help "number of warmup run",
             ratio = 0.5 &= help "Ratio",
             initial = 0 &= help "Initial size",
             seed = 4096 &= help "Seed",
             file = "report" &= help "Report file prefix",
             runs = 25 &= help "Number of runs",
             bench = "" &= help "Benchvariant"}
