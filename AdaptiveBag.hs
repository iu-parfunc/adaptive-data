{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module AdaptiveBag
       (
         AdaptiveBag
       , newBag
       , newBagThreshold
       , add
       , remove
       )
       where

import Control.Concurrent
import Control.Monad
import Data.Atomics
import Data.IORef
import Unsafe.Coerce

import qualified ScalableBag as SB
import qualified PureBag as PB
import EntryRef

data Hybrid a = A !(PB.PureBag a) Int
              | AB !(PB.PureBag a) !(SB.ScalableBag a) Int
              | B !(SB.ScalableBag a) Int

type AdaptiveBag a = IORef (Hybrid a)

newBag :: IO (AdaptiveBag a)
newBag = newBagThreshold 10

newBagThreshold :: Int -> IO (AdaptiveBag a)
newBagThreshold thresh = do
  pbag <- PB.newPureBag
  newIORef $ A pbag thresh

-- Push onto the bag.
add :: AdaptiveBag a -> a -> IO ()
add bag x = do
  tick <- readForCAS bag
  case peekTicket tick of
    A pbag thresh ->
      let loop 0 = transition bag >> add bag x
          loop i = do
            tick <- readForCAS bag
            case peekTicket tick of
              A pbag _ -> do
                result <- PB.add pbag x
                case result of
                  Just x -> return ()
                  Nothing -> loop (i-1)
              _ -> add bag x
      in loop thresh
    AB pbag sbag _ -> do
      result <- SB.add sbag x
      case result of
        Just x -> return ()
        Nothing -> add bag x
    B sbag _ -> do
      result <- SB.add sbag x
      case result of
        Just x -> return ()
        Nothing -> add bag x


-- Attempt to pop from the bag, returning Nothing if it's empty.
remove :: AdaptiveBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    A pbag thresh -> 
      let loop 0 = transition bag >> remove bag
          loop i = do
            tick <- readForCAS bag
            case peekTicket tick of
              A pbag _ -> do
                result <- PB.remove pbag
                case result of
                  Just x -> return (Just x)
                  Nothing -> do
                    empty <- PB.empty pbag
                    if empty
                      then return Nothing
                      else loop (i-1)
              _ -> remove bag
      in loop thresh
    AB pbag sbag thresh -> do
      result <- PB.remove pbag
      case result of
        Just x -> return (Just x)
        Nothing -> do
          empty <- PB.empty pbag
          if empty
            then casIORef bag tick (B sbag thresh) >> remove bag
            else remove bag
    B sbag _ -> do
      result <- SB.remove sbag
      case result of
        Just x -> return (Just x)
        Nothing -> do
          empty <- SB.empty sbag
          if empty
            then return Nothing
            else remove bag

transition :: AdaptiveBag a -> IO ()
transition bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    A pbag thresh -> do
      sbag <- SB.newScalableBag
      (success, _) <- casIORef bag tick (AB pbag sbag thresh)
      if success
        then transition bag
        else return ()
    AB pbag sbag _ -> do
      PB.transition pbag
        (\x -> case x of
           Val v -> Copied v
           Copied v -> Copied v)
    _ -> return ()
