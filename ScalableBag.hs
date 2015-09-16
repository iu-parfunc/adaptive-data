{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module ScalableBag
       (
         ScalableBag
       , newScalableBag
       , add
       , remove
       , osThreadID
       , empty
       )
       where

import Control.Concurrent
import Data.Atomics
import Data.Bits
import qualified Data.Atomics.Counter as C
import Data.TLS.PThread
import Data.Primitive.Array
import System.IO.Unsafe (unsafePerformIO)
import EntryRef
import Control.Monad.ST

dbgPrint :: String -> IO ()
#if 1
dbgPrint s = putStrLn $ " [dbg] "++s
#else
dbgPrint _ = return ()
{-# INLINE dbgPrint #-}
#endif

--------------------------------------------------------------------------------

type SBag a = MutableArray RealWorld (EntryVal [a])
type ScalableBag a = (SBag a, Int)

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

newScalableBag :: IO (ScalableBag a)
newScalableBag = do
  caps <- getNumCapabilities
  let length = padFactor * caps
  array <- newArray (padFactor * caps) (Val [])
  let bag = (array, length)
  return bag

empty :: ScalableBag a -> IO (Bool)
empty (bag, length) =
  let loop i | i >= length = return True
      loop i = do
        tick <- readArrayElem bag i
        case peekTicket tick of
          Val (x:xs) -> return False
          Copied (x:xs) -> return False
          _ -> loop $ i+1
  in loop 0

add :: ScalableBag a -> a -> IO (Maybe a)
add (bag, length) x = do
  tid <- getTLS osThreadID
  let len = padDiv length
  dbgPrint$ "tid "++show tid++"Length of bag: "++show len
  -- We try to keep this collision-free:
  let idx = tid `mod` len
      idx' = padFactor * idx
  dbgPrint$ "tid "++show tid++" going into CAS loop on index "++show idx'
  pushArray bag idx' x

remove :: ScalableBag a -> IO (Maybe a)
remove (bag, length) = do
  tid <- getTLS osThreadID
  let idx = tid `mod` length
      retryLoop vec ix start | ix >= length = retryLoop vec 0 start
      retryLoop vec ix start = do
        tick <- readArrayElem vec ix;
        let ix' = ix + 1 in
          case peekTicket tick of
          Val v -> case v of
            [] -> 
              if ix' == start || (ix' >= length && start == 0)
              then return Nothing -- looped around once, nothing to pop
              else retryLoop vec ix' start -- keep going
            _ -> do
              res <- popArray vec ix pop
              case res of
                Nothing -> retryLoop vec ix' start -- someone else stole what we were going to pop
                jx -> return jx
          Copied v -> case v of 
            [] -> 
              if ix' == start || (ix' >= length && start == 0)
              then return Nothing -- looped around once, nothing to pop
              else retryLoop vec ix' start -- keep going
            _ -> do
              res <- popArray vec ix pop
              case res of
                Nothing -> retryLoop vec ix' start -- someone else stole what we were going to pop
                jx -> return jx
                
      pop v = case v of
        Val (x:xs) -> Just (x, Val xs)
        Copied (x:xs) -> Just (x, Val xs)
        _ -> Nothing
  retryLoop bag idx idx

{-# INLINE pushArray #-}
pushArray :: SBag a -> Int -> a -> IO (Maybe a)
pushArray ary ix x = 
  do !tick <- readArrayElem ary ix;
     case peekTicket tick of
       Val xs ->
         let !newVal = Val $ x:xs
         in do
           (success, tick') <- casArrayElem ary ix tick newVal
           dbgPrint$ "push CAS["++show ix++"]="++show success
           if success
             then return (Just x)
             else return Nothing
       Copied _ ->
         return Nothing

{-# INLINE popArray #-}
popArray :: SBag a -> Int -> (EntryVal [a] -> Maybe(a, EntryVal [a])) -> IO (Maybe a)
popArray vec ix f =
  do !tick <- readArrayElem vec ix
     let !newVal = f $ peekTicket tick
     case newVal of
       Just (x, xs) -> do
         (success, tick') <- casArrayElem vec ix tick xs
         dbgPrint$ "pop: CAS["++show ix++"]="++show success
         if success
           then return (Just x)
           else return Nothing
       Nothing ->
         return Nothing
