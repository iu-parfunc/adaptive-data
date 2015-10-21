{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main
       where

import Control.Concurrent
import Control.Concurrent.Async
--import Control.Monad
import Control.Concurrent.Extra
--import Data.Atomics
--import Data.Atomics.Vector
--import qualified Data.Atomics.Counter as C
import Data.Int
import Data.IORef
--import qualified Data.Vector.Mutable as VM
--import qualified Data.Vector.Storable as VS
--import System.Environment
import System.Console.CmdArgs
--import System.IO.Unsafe
import System.IO
import System.Random
--import System.Posix.Unistd
import Data.Time.Clock

--import qualified Data.TLS.PThread

import qualified AdaptiveBag as AB
import qualified Data.Concurrent.PureBag as PB
import qualified Data.Concurrent.ScalableBag as SB

thread :: Int -> (Int64 -> IO()) -> (() -> IO(Maybe Int64)) -> Flag -> Barrier Bool -> IORef(Bool) -> Int -> IO(Int)
thread tid ws rs option bar cont seedn = do
  let loop i g = do
        c <- readIORef cont
        if c
          then do
          let (p, g') = randomR (0.0, 1.0) g :: (Double, StdGen)
          let (a, g'')= random g' :: (Int64, StdGen)
          if p> ratio option
            then ws a
            else do
            !r <- rs ()
            return ()
          loop (i+1) g''
          else return (i)

--  putStrLn $ "Thread " ++ show tid ++ "started"
  !b <- waitBarrier bar
--  putStrLn $ "Thread id: " ++ show tid ++  ", seed: " ++ show seedn
  if b
    then loop 0 $ mkStdGen seedn
    else return (0)

test :: Int -> (Int64 -> IO()) -> (() -> IO(Maybe Int64)) -> Flag -> IO (Double)
test threadn ws rs option = do
  !bar <- newBarrier
  !ref <- newIORef True
  !asyncs <- mapM (\tid -> do
                       s <- randomIO
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
{-
run_pb :: Int -> Flag -> IO()
run_pb threadn option = do
  outh <- openFile ((file option) ++ "_pb.csv") WriteMode
  hPutStrLn outh "threadn,Mean,Stddev"
  setStdGen $ mkStdGen $ seed option

  let loop t i | i>0 = do
                   !bag <- PB.newBag
                   !ops <- test t (PB.add bag) (\() -> PB.remove bag) option
                   putStrLn $ "PureBag: " ++ show t ++ " threads, " ++ show ops ++ " ops/s"
                   hFlush stdout
                   xs <- (loop t $ i-1)
                   return $ ops : xs
      loop _ _ = return []

  let loop_n n | n <= threadn = do
                   res <- loop n $ runs option
                   hPutStrLn outh $ (show n) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
                   loop_n $ n+1
      loop_n _ = return ()

  loop_n 1
  hClose outh

run_sb :: Int -> Flag -> IO()
run_sb threadn option = do
  outh <- openFile ((file option) ++ "_sb.csv") WriteMode
  hPutStrLn outh "threadn,Mean,Stddev"
  setStdGen $ mkStdGen $ seed option

  let loop t i | i>0 = do
                   !bag <- SB.newBag
                   !ops <- test t (SB.add bag) (\() -> SB.remove bag) option
                   putStrLn $ "ScalableBag: " ++ show t ++ " threads, " ++ show ops ++ " ops/s"
                   hFlush stdout
                   xs <- (loop t $ i-1)
                   return $ ops : xs
      loop _ _ = return []

  let loop_n n | n <= threadn = do
                   res <- loop n $ runs option
                   hPutStrLn outh $ (show n) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
                   loop_n $ n+1
      loop_n _ = return ()

  loop_n 1
  hClose outh

run_ab :: Int -> Flag -> IO()
run_ab threadn option = do
  outh <- openFile ((file option) ++ "_ab.csv") WriteMode
  hPutStrLn outh "threadn,Mean,Stddev"
  setStdGen $ mkStdGen $ seed option

  let loop t i | i>0 = do
                   !bag <- AB.newBagThreshold 1
                   !ops <- test t (AB.add bag) (\() -> AB.remove bag) option
                   putStrLn $ "AdaptiveBag: " ++ show t ++ " threads, " ++ show ops ++ " ops/s"
                   hFlush stdout
                   xs <- (loop t $ i-1)
                   return $ ops : xs
      loop _ _ = return []

  let loop_n n | n <= threadn = do
                   res <- loop n $ runs option
                   hPutStrLn outh $ (show n) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
                   loop_n $ n+1
      loop_n _ = return ()

  loop_n 1
  hClose outh

run_nop :: Int -> Flag -> IO()
run_nop threadn option = do
  outh <- openFile ((file option) ++ "_nop.csv") WriteMode
  hPutStrLn outh "threadn,Mean,Stddev"
  setStdGen $ mkStdGen $ seed option

  let loop t i | i>0 = do
                   !ops <- test t (\_ -> do return ()) (\() -> return Nothing) option
                   putStrLn $ "NOP: " ++ show t ++ " threads, " ++ show ops ++ " ops/s"
                   hFlush stdout
                   xs <- (loop t $ i-1)
                   return $ ops : xs
      loop _ _ = return []

  let loop_n n | n <= threadn = do
                   res <- loop n $ runs option
                   hPutStrLn outh $ (show n) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
                   loop_n $ n+1
      loop_n _ = return ()

  loop_n 1
  hClose outh
-}

run :: Int -> Flag -> IO()
run threadn option = do
  outh <- openFile ((file option) ++ "_" ++ (bench option) ++ ".csv") AppendMode
--  hPutStrLn outh "threadn,Mean,Stddev"
  setStdGen $ mkStdGen $ seed option

  let loop i | i>0 = do
                 !ops <- case bench option of
                   "pure" -> do
                     !bag <- PB.newBag
                     test threadn (PB.add bag) (\() -> PB.remove bag) option
                   "scalable" -> do
                     !bag <- SB.newBag
                     test threadn (SB.add bag) (\() -> SB.remove bag) option
                   "adaptive" -> do
                     !bag <- AB.newBag
                     test threadn (AB.add bag) (\() -> AB.remove bag) option
                   _ -> test threadn (\_ -> do return ()) (\() -> return Nothing) option
                     
                 putStrLn $ ": " ++ show threadn ++ " threads, " ++ show ops ++ " ops/ms"
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
{-
  run_nop threadn option
  run_pb threadn option
  run_sb threadn option
  run_ab threadn option-}
  if length (bench option) == 0
    then return ()
    else run threadn option
  return ()
  

data Flag
  = Flag {duration :: Int,
          ratio :: Double,
          initial :: Int,
          seed :: Int,
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
