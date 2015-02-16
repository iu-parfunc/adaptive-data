{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Concurrent.ScalableBag
       (
         ScalableBag
       , newBag
       , add
       , remove
       , osThreadID
       )
       where

import Control.Applicative
import Control.Concurrent
import Data.Atomics
import Data.Atomics.Vector
import qualified Data.Atomics.Counter as C
import Data.IORef
import Data.TLS.PThread
import Data.Vector.Mutable as V
import System.IO.Unsafe (unsafePerformIO)

type ScalableBag a = IOVector [a]

{-# NOINLINE osThreadID #-}
osThreadID :: TLS Int
osThreadID = unsafePerformIO $ mkTLS $ C.incrCounter 1 threadCounter

{-# NOINLINE threadCounter #-}
threadCounter :: C.AtomicCounter
threadCounter = unsafePerformIO $ C.newCounter 0

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
  V.replicate caps []

add :: ScalableBag a -> a -> IO ()
add bag x = do
  tid <- getTLS osThreadID
  let idx = tid `mod` V.length bag
  tick <- unsafeReadVectorElem bag idx
  casVectorLoop_ bag (x:) idx

remove :: ScalableBag a -> IO (Maybe a)
remove bag = do
  tid <- getTLS osThreadID
  let idx = tid `mod` V.length bag
      retryLoop vec ix start | ix >= V.length vec = retryLoop vec 0 start
      retryLoop vec ix start = do
        tick <- unsafeReadVectorElem bag ix
        case peekTicket tick of
          [] -> let ix' = ix + 1 in
            if ix' == start
            then return Nothing -- looped around once, nothing to pop
            else if ix' >= V.length vec && start == 0
                 then return Nothing -- looped around once, nothing to pop
                 else retryLoop vec ix' start -- keep going
          _ -> do
            res <- casVectorLoop bag pop ix
            case res of
              Nothing -> retryLoop vec ix start -- someone else stole what we were going to pop
              jx -> return jx
      pop [] = ([], Nothing)
      pop (x:xs) = (xs, Just x)
  retryLoop bag idx idx

casVectorLoop_ :: IOVector a -> (a -> a) -> Int -> IO ()
casVectorLoop_ vec f ix = casVectorLoop vec f' ix
  where f' x = (f x, ())

casVectorLoop :: IOVector a -> (a -> (a, b)) -> Int -> IO b
casVectorLoop vec f ix = retryLoop =<< readVectorElem vec ix
  where retryLoop tick = do
          let (new, ret) = f $! peekTicket tick
          (success, tick') <- casVectorElem vec ix tick new
          if success then return ret else retryLoop tick'
