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
import Data.IORef
--import qualified Data.Vector.Mutable as VM
--import qualified Data.Vector.Storable as VS
--import System.Environment
import System.Console.CmdArgs
--import System.IO.Unsafe
--import System.IO
--import System.Random
--import System.Posix.Unistd
import Data.Time.Clock

--import qualified Data.TLS.PThread

--import qualified AdaptiveBag as AB
import qualified Data.Concurrent.PureBag as PB
--import qualified Data.Concurrent.ScalableBag  as SB

test :: Int -> (Int -> IO()) -> (() -> IO(Maybe Int)) -> Flag -> Barrier b -> IORef(Bool) -> IO(Int)
test tid ws rs option bar cont = do
  let loop i = do
        c <- readIORef cont
        if c then
          do ws i
             loop $ i+1
          else return (i)
  putStrLn $ "Thread " ++ show tid ++ "started"
  !b <- waitBarrier bar
  putStrLn $ "Thread id: " ++ show tid
  loop 0

timeout :: Int -> IO()
timeout t = threadDelay t

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number: " ++ show threadn
  putStrLn $ "Duration:      " ++ show (duration option) ++ "s"
  putStrLn $ "Ratio:         " ++ show (ratio option)
  putStrLn $ "Initial size:  " ++ show (initial option)
  putStrLn $ "Seed:          " ++ show (seed option)
  !bar <- newBarrier
  !bag <- PB.newBag
  !ref <- newIORef True
  !asyncs <- mapM (\tid -> async $ test tid (PB.add bag) (\() -> PB.remove bag) option bar ref) [1..threadn]
  signalBarrier bar True
  !start <- getCurrentTime
  threadDelay $ 1000000 * duration option
  writeIORef ref False
  !end <- getCurrentTime
  !res <- mapM wait asyncs

  print $ sum res
  print $ (realToFrac $ diffUTCTime end start)
  print $ (fromIntegral $ sum res)
  print $ (fromIntegral $ sum res) / (realToFrac $ diffUTCTime end start)
  return ()
  

data Flag
  = Flag {duration :: Int,
          ratio :: Float,
          initial :: Int,
          seed :: Int}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {duration = 100 &= help "Duration",
             ratio = 0.5 &= help "Ratio",
             initial = 0 &= help "Initial size",
             seed = 4096 &= help "Seed"}
