{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This version is an experiment to try to figure out what is
-- destroying performance in ScalableBag

module Data.Concurrent.ScalableBagBoxed
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

import qualified Data.Concurrent.PureBag as PB

--------------------------------------------------------------------------------

type ScalableBag a = IOVector (Maybe (PB.PureBag a))

{-# NOINLINE osThreadID #-}
osThreadID :: TLS Int
osThreadID = unsafePerformIO $ mkTLS $ C.incrCounter 1 threadCounter

{-# NOINLINE threadCounter #-}
threadCounter :: C.AtomicCounter
threadCounter = unsafePerformIO $ C.newCounter 0

newBag :: IO (ScalableBag a)
newBag = do
  caps <- getNumCapabilities
--  generateM caps (\_ -> fmap Just PB.newBag)
  V.replicate caps Nothing


add :: ScalableBag a -> a -> IO ()
add bag x = do
  tid <- getTLS osThreadID
  -- We try to keep this collision-free:
  let idx = tid `mod` (V.length bag)
  bg <- V.unsafeRead bag idx  
  case bg of
    Just b  -> PB.add b x
    Nothing -> do tick <- unsafeReadVectorElem bag idx
                  case peekTicket tick of
                    Just b -> PB.add b x
                    Nothing -> do
                      !b <- PB.newBag
                      (s,nt) <- casVectorElem bag idx tick (Just b)
                      -- If we fail it's only because someone else wrote it:
                      if s
                        then PB.add b x
                        else case peekTicket nt of
                               Nothing -> error "IMPOSSIBLE!"
                               Just b2 -> PB.add b2 x


remove :: ScalableBag a -> IO (Maybe a)
remove bag = error "FINISHME - remove"
{-
remove bag = do
  tid <- getTLS osThreadID
  let idx = tid `mod` V.length bag
      retryLoop vec ix start | ix >= V.length vec = retryLoop vec 0 start
      retryLoop vec ix start = do
        tick <- unsafeReadVectorElem bag ix
        case peekTicket tick of
          [] -> let ix' = ix + 1 in
            if ix' == start || (ix' >= V.length vec && start == 0)
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

{-# INLINE casVectorLoop_ #-}
casVectorLoop_ :: IOVector a -> (a -> a) -> Int -> IO ()
casVectorLoop_ vec f ix = retryLoop =<< readVectorElem vec ix
  where retryLoop tick = do
          let !val    = peekTicket tick
              !newVal = f val
          (success, tick') <- casVectorElem vec ix tick newVal
          if success then return () else retryLoop tick'

{-# INLINE casVectorLoop #-}
casVectorLoop :: IOVector a -> (a -> (a, b)) -> Int -> IO b
casVectorLoop vec f ix = retryLoop =<< readVectorElem vec ix
  where retryLoop tick = do
          let !val = peekTicket tick
              (!newVal, !ret) = f val
          (success, tick') <- casVectorElem vec ix tick newVal
          if success then return ret else retryLoop tick'
-}

-- FIXME: Vector.Mutable should really have generateM:
generateM :: Int -> (Int -> a) -> IO (IOVector a)
generateM num fn = do
  vec <- V.new num
  for_ 0 (num-1) $ \ix ->
    V.unsafeWrite vec ix (fn ix)
  return vec


-- Inclusive/Inclusive
for_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
-- for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
