{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module ScalableBag
       (
         ScalableBag
       , newBag
       , add
       , remove
       , osThreadID
       )
       where

import Control.Concurrent
import Data.Atomics
import Data.Bits
import Data.Atomics.Vector
import qualified Data.Atomics.Counter as C
import Data.TLS.PThread
import Data.Vector.Mutable as V
import System.IO.Unsafe (unsafePerformIO)
import EntryRef

dbgPrint :: String -> IO ()
#if 1
dbgPrint s = putStrLn $ " [dbg] "++s
#else
dbgPrint _ = return ()
{-# INLINE dbgPrint #-}
#endif

--------------------------------------------------------------------------------

type ScalableBag a = IOVector (EntryVal [a])

{-# NOINLINE osThreadID #-}
osThreadID :: TLS Int
osThreadID = unsafePerformIO $ mkTLS $ C.incrCounter 1 threadCounter

{-# NOINLINE threadCounter #-}
threadCounter :: C.AtomicCounter
threadCounter = unsafePerformIO $ C.newCounter 0

-- | This padding factor prevents false sharing.  Cache lines on intel
-- are 64 bytes / 8 words.
padFactor :: Int
padFactor = 8

-- | Divide by the padding factor
padDiv :: Int -> Int
padDiv x = shiftR x 3

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
  V.replicate (padFactor * caps) (Val [])

add :: ScalableBag a -> a -> IO (Maybe a)
add bag x = do
  tid <- getTLS osThreadID
  let len = padDiv (V.length bag)
  dbgPrint$ "tid "++show tid++"Length of bag: "++show len
  -- We try to keep this collision-free:
  let idx = tid `mod` len
      idx' = padFactor * idx
  dbgPrint$ "tid "++show tid++" going into CAS loop on index "++show idx'
  insVector bag x idx'

remove :: ScalableBag a -> IO (Maybe a)
remove bag = do
  tid <- getTLS osThreadID
  let idx = tid `mod` V.length bag
      retryLoop vec ix start | ix >= V.length vec = retryLoop vec 0 start
      retryLoop vec ix start = do
        tick <- unsafeReadVectorElem bag ix;
        case peekTicket tick of
          Val v -> case v of
            [] -> let ix' = ix + 1 in
              if ix' == start || (ix' >= V.length vec && start == 0)
              then return Nothing -- looped around once, nothing to pop
              else retryLoop vec ix' start -- keep going
            _ -> do
              res <- popVector bag ix
              case res of
                Nothing -> retryLoop vec ix start -- someone else stole what we were going to pop
                jx -> return jx
          Copied _ ->
            let ix' = ix + 1 in
            if ix' == start || (ix' >= V.length vec && start == 0)
            then return Nothing -- looped around once, nothing to pop
            else retryLoop vec ix' start -- keep going
  retryLoop bag idx idx

{-# INLINE insVector #-}
insVector :: ScalableBag a -> a -> Int -> IO (Maybe a)
insVector vec x ix = 
  do !tick <- readVectorElem vec ix;
     case peekTicket tick of
       Val v ->
         let !newVal = Val $ x:v
         in do
           dbgPrint$ "ix "++show ix
           (success, tick') <- casVectorElem vec ix tick newVal;
           dbgPrint$ show success
           if success
             then return (Just x)
             else return Nothing
       Copied _ ->
         return Nothing

{-# INLINE popVector #-}
popVector :: ScalableBag a -> Int -> IO (Maybe a)
popVector vec ix =
  do !tick <- readVectorElem vec ix
     case peekTicket tick of
          Val v ->
            case v of
            x:xs -> do
              (success, tick') <- casVectorElem vec ix tick $ Val xs
              if success
                then return (Just x)
                else return Nothing
            [] -> return Nothing
          Copied _ -> return Nothing

