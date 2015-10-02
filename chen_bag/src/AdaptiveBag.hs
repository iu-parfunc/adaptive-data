{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module AdaptiveBag
       (
         AdaptiveBag
       , newBag
       , newBagThreshold
       , add
       , remove
       )
       where

-- import Control.Concurrent
-- import Control.Monad
import Data.Atomics
import Data.IORef
-- import Unsafe.Coerce

import qualified ScalableBag as SB
import qualified PureBag as PB
import EntryRef

dbgPrint :: String -> IO ()
#if 1
dbgPrint s = putStrLn $ " [dbg] "++s
#else
dbgPrint _ = return ()
{-# INLINE dbgPrint #-}
#endif

data Hybrid a = A !(PB.PureBag a) Int
              | AB !(PB.PureBag a) !(SB.ScalableBag a) Int
              | B !(SB.ScalableBag a) Int
              | BA !(SB.ScalableBag a) !(PB.PureBag a) Int

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
    A _ thresh ->
      let loop 0 tick' = transition bag tick' >> add bag x
          loop i tick' = do
            case peekTicket tick' of
              A pbag _ -> do
                result <- PB.add pbag x
                if result
                  then return ()
                  else do tick'' <- readForCAS bag
                          loop (i-1) tick''
              _ -> add bag x
      in loop thresh tick
    AB _ sbag _ -> do
      result <- SB.add sbag x
      if result 
        then return ()
        else add bag x
    B sbag _ -> do
      result <- SB.add sbag x
      if result 
        then return ()
        else add bag x
    BA _ pbag _ -> do
      result <- PB.add pbag x
      if result
        then return ()
        else add bag x

-- Attempt to pop from the bag, returning Nothing if it's empty.
remove :: AdaptiveBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
    A _ thresh ->
      let loop 0 tick' = transition bag tick' >> remove bag
          loop i tick' = do
            case peekTicket tick' of
              A pbag _ -> do
                result <- PB.remove pbag
                case result of
                  Just x -> return (Just x)
                  Nothing -> do
                    empty <- PB.empty pbag
                    if empty
                      then return Nothing
                      else do tick'' <- readForCAS bag
                              loop (i-1) tick''
              _ -> remove bag
      in loop thresh tick
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
    BA sbag pbag thresh -> do
      result <- SB.remove sbag
      case result of
        Just x -> return (Just x)
        Nothing -> do
          empty <- SB.empty sbag
          if empty
            then casIORef bag tick (A pbag thresh) >> remove bag
            else remove bag

transition :: AdaptiveBag a -> Ticket (Hybrid a) -> IO ()
transition bag tick = do
  let mark x = case x of
        Val v -> (True, Copied v)
        Copied v -> (False, Copied v)
  case peekTicket tick of
    A pbag thresh -> do
      sbag <- SB.newScalableBag
      (success, tick') <- casIORef bag tick (AB pbag sbag thresh)
      dbgPrint$ "transition: A->AB" ++ show success
      if success
        then transition bag tick'
        else return ()
    AB pbag _ _ -> do
      dbgPrint$ "transition: AB->B"
      PB.transition pbag mark
    B sbag thresh ->  do
      pbag <- PB.newPureBag
      (success, tick') <- casIORef bag tick (BA sbag pbag thresh)
      dbgPrint$ "transition: B->BA"++show success
      if success
        then transition bag tick'
        else return ()
    BA sbag _ _ -> do
      dbgPrint$ "transition: BA->A"
      SB.transition sbag mark
