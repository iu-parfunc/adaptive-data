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
import System.Mem
import Data.Time.Clock
import GHC.Word
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified Control.Concurrent.PureMap as PM
import qualified Control.Concurrent.PureMapL as PML
import qualified Control.Concurrent.Map as CM
import qualified AdaptiveMap as AM

thread :: Int -> [Int64 -> Int64 -> IO()] -> [Double] -> Flag -> Barrier Bool -> IORef(Bool) -> Word64 -> IO(Int)
thread !_ !ops !ratios !option !bar !cont !seedn = do
  !g <- PCG.restore $ PCG.initFrozen seedn
  let loop !i = do
        !c <- readIORef cont
        if c
          then do
          !p <- PCG.uniformRD (0.0, 1.0) g :: IO Double
          !k <- PCG.uniformRI64 (0, range option) g :: IO Int64
          !v <- PCG.uniformRI64 (0, range option) g :: IO Int64
          let op = ops !! snd (foldl (\(z,j) -> \x ->
                                       if (x+z>=p)
                                       then (x+z,j)
                                       else (x+z,j+1))
                               (0.0::Double,0) ratios)
          op k v
          loop $ i+1
          
          else return i

--  putStrLn $ "Thread " ++ show tid ++ "started"
  !b <- waitBarrier bar
--  putStrLn $ "Thread id: " ++ show tid ++  ", seed: " ++ show seedn
  if b
    then loop 0
    else return 0

test :: Int -> [Int64 -> Int64 -> IO()] -> [Double] -> Flag -> PCG.GenIO -> IO (Double)
test !threadn !ops !ratios !option !gen = do
  performGC
  !bar <- newBarrier
  !ref <- newIORef True
  !asyncs <- mapM (\tid -> do
                       s <- PCG.uniformW64 gen :: IO Word64
                       async $ thread tid ops ratios option bar ref s) [1..threadn]
  signalBarrier bar True
  !start <- getCurrentTime
  threadDelay $ 1000 * duration option
  writeIORef ref False
  !end <- getCurrentTime
  !res <- mapM wait asyncs
  return $ ((fromIntegral $ sum res)::Double) / ((realToFrac $ diffUTCTime end start) * 1000000.0 ::Double)

mean :: [Double] -> Double
mean xs = (sum xs) / ((realToFrac $ length xs) :: Double)

stddev :: [Double] -> Double
stddev xs = sqrt $ (sum (map (\x -> (x - avg) * (x - avg)) xs)) / len
  where len = (realToFrac $ length xs) :: Double
        avg = mean xs

run :: Int -> Flag -> IO()
run threadn option = do
  let fileName = (file option) ++ "_" ++ (bench option)
  outh <- openFile (fileName ++ ".csv") AppendMode
--  hPutStrLn outh "threadn,Mean,Stddev"
--  setStdGen $ mkStdGen $ seed option
  !gen <- PCG.restore $ PCG.initFrozen $ seed option
  let ratios = [gratio option, iratio option, 1.0 - (gratio option) - (iratio option)]

  let loop !i | i>0 = do
                 !ops <- case bench option of
                   "pure" -> do
                     !m <- PM.newMap
                     test threadn [(\k _ -> do !r <- PM.get k m ; return ()),
                                   (\k v -> PM.ins k v m),
                                   (\k _ -> PM.del k m)] ratios option gen
                   "pureL" -> do
                     !m <- PML.newMap
                     test threadn [(\k _ -> do !r <- PML.get k m ; return ()),
                                   (\k v -> PML.ins k v m),
                                   (\k _ -> PML.del k m)] ratios option gen
                   "ctrie" -> do
                     !m <- CM.empty
                     test threadn [(\k _ -> do !r <- CM.lookup k m ; return ()),
                                   (\k v -> CM.insert k v m),
                                   (\k _ -> CM.delete k m)] ratios option gen
                   "adaptive" -> do
                     !m <- AM.newMap fileName threadn
                     test threadn [(\k _ -> do !r <- AM.get k m ; return ()),
                                   (\k v -> AM.ins k v m),
                                   (\k _ -> AM.del k m)] ratios option gen
                   "nop" -> test threadn [nop, nop, nop] ratios option gen
                     where nop _ _ = do return ()
                   _ -> undefined
                     
                 putStrLn $ show threadn ++ " threads: " ++ show ops ++ " ops/ms"
                 hFlush stdout
                 !xs <- (loop $ i-1)
                 return $ ops : xs
      loop _ = return []

  !xs <- loop $ (runs option) + (warmup option)
  let res = drop (warmup option) xs
  hPutStrLn outh $ (show threadn) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
  hClose outh

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number: " ++ show threadn
  putStrLn $ "Duration:      " ++ show (duration option) ++ "ms"
  putStrLn $ "Get Ratio:     " ++ show (gratio option)
  putStrLn $ "Delete Ratio:  " ++ show (1.0 - (gratio option) - (iratio option))
  putStrLn $ "Insert Ratio:  " ++ show (iratio option)
  putStrLn $ "Initial size:  " ++ show (initial option)
  putStrLn $ "Number of runs:" ++ show (runs option)
  putStrLn $ "Warmup runs:   " ++ show (warmup option)
  putStrLn $ "Seed:          " ++ show (seed option)
  putStrLn $ "File:          " ++ show (file option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={nop, pure, pureL, ctrie, adaptive})"
    else run threadn option
  return ()
  

data Flag
  = Flag {duration :: Int,
          gratio :: Double,
          iratio :: Double,
          initial :: Int,
          seed :: Word64,
          file :: String,
          runs :: Int,
          warmup :: Int,
          bench :: String,
          range :: Int64}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {duration = 100 &= help "Duration",
             warmup = 5 &= help "number of warmup run",
             gratio = 1.0/3.0 &= help "Get Ratio",
             iratio = 1.0/3.0 &= help "Insert ratio",
             initial = 0 &= help "Initial size",
             range = 10000 &= help "Key range",
             seed = 4096 &= help "Seed",
             file = "report" &= help "Report file prefix",
             runs = 25 &= help "Number of runs",
             bench = "" &= help "Benchvariant {nop, pure, pureL, ctrie, adaptive}"}
