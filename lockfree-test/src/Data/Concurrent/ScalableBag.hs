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
import Data.TLS.GHC
import qualified Data.TLS.PThread as PThread
import Data.Vector.Mutable as V
import System.IO.Unsafe (unsafePerformIO)

data ScalableBag a = ScalableBag {
  tls :: TLS TLSData
  , bag :: (IOVector [a])
  }

data TLSData = TLSData {
  idx :: !Int
  , lastRemoved :: !(IORef Int)
  }

{-# NOINLINE osThreadID #-}
osThreadID :: PThread.TLS Int
osThreadID = unsafePerformIO $ PThread.mkTLS $ C.incrCounter 1 threadCounter

{-# NOINLINE threadCounter #-}
threadCounter :: C.AtomicCounter 
threadCounter = unsafePerformIO $ C.newCounter 0

-- foreign import ccall gettid :: IO Int 

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
  tid <- myThreadId
  (idx, _) <- threadCapability tid  
  tls <- mkTLS $ TLSData idx <$> newIORef idx
  ScalableBag tls <$> V.replicate caps []

add :: ScalableBag a -> a -> IO ()
add ScalableBag{tls, bag} x = do
  TLSData{idx} <- getTLS tls
  tick <- unsafeReadVectorElem bag idx
  casVectorLoop_ bag (x:) idx

remove :: ScalableBag a -> IO (Maybe a)
remove ScalableBag{tls,bag} = do
  TLSData{lastRemoved} <- getTLS tls
  let retryLoop vec ix start | ix >= V.length vec = retryLoop vec 0 start
      retryLoop vec ix start = do
        tick <- unsafeReadVectorElem bag ix
        case peekTicket tick of
          [] -> let ix' = succ ix in
            if ix' == start
            then return Nothing -- looped around once, nothing to pop
            else if ix' >= V.length vec && start == 0
                 then return Nothing -- looped around once, nothing to pop
                 else retryLoop vec (ix+1) start -- keep going
          _ -> writeIORef (lastRemoved) ix >> casVectorLoop bag pop ix
      pop (x:xs) = (xs, Just x)
  idx <- readIORef lastRemoved
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
