{-# LANGUAGE BangPatterns #-}

module Data.Concurrent.AdaptiveBag
       (
         AdaptiveBag
       , newBag
       , add
       , remove
       )
       where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad
import Data.Atomics
import Data.Atomics.Vector
import Data.IORef
import qualified Data.Vector.Mutable as V
import Unsafe.Coerce

import Data.Concurrent.ScalableBag (ScalableBag)
import qualified Data.Concurrent.ScalableBag as SB

data Hybrid a = Pure ![a]
              | Trans ![a] !(ScalableBag a)
              | LockFree !(ScalableBag a)

type AdaptiveBag a = IORef (Hybrid a)

newBag :: IO (AdaptiveBag a)
newBag = newBagThreshold 10

newBagThreshold :: Int -> IO (AdaptiveBag a)
newBagThreshold thresh = newIORef $ Pure []

-- Push onto the bag.
add :: AdaptiveBag a -> a -> IO ()
add bag x = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure xs -> do
      (success, _) <- casIORef bag tick $ Pure (x:xs)
      unless success $ transition bag >> add bag x -- make sure this write isn't dropped
    Trans xs bag -> SB.add bag x
    LockFree bag -> SB.add bag x

-- Attempt to pop from the bag, returning Nothing if it's empty.
remove :: AdaptiveBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure [] -> return Nothing
    Pure (x:xs) -> do
      (success, _) <- casIORef bag tick $ Pure xs
      if success
        then return $ Just x
        else transition bag >> remove bag -- make sure this write isn't dropped
    Trans xs bag -> SB.remove bag
    LockFree bag -> SB.remove bag

transition :: AdaptiveBag a -> IO ()
transition bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    Pure xs -> do
      caps <- getNumCapabilities
      scalable <- SB.newBag
      (success, _) <- casIORef bag tick (Trans xs scalable)
      when success $ do
        putStrLn $ "[TRANSITION]" ++ show (unsafeCoerce bag :: Int) -- beware!
        forkIO $ forM_ xs (SB.add scalable)
        casLoop_ bag (const $ LockFree scalable)
    _ -> return ()

casLoop_ :: IORef a -> (a -> a) -> IO ()
casLoop_ ref f = casLoop ref f'
  where f' x = (f x, ())

casLoop :: IORef a -> (a -> (a, b)) -> IO b
casLoop ref f = retryLoop =<< readForCAS ref
  where retryLoop tick = do
          let (new, ret) = f $! peekTicket tick
          (success, tick') <- casIORef ref tick new
          if success then return ret else retryLoop tick'

-- Return the index in the vector that this thread should access.
getIndex :: IO Int
getIndex = do
  tid <- myThreadId
  (idx, _) <- threadCapability tid
  return idx
