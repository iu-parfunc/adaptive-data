{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Adaptive.AdaptiveMap (
    AdaptiveMap,
    newMap, newBMap,
    get,
    ins,
    del,
    transition,
    fromList,
    size
    ) where

import qualified Control.Concurrent.Adaptive.Ctrie as CM
import qualified Control.Concurrent.PureMap        as PM
import           Control.Exception
import           Control.Monad
import           Data.Atomics
import qualified Data.Concurrent.IORef             as FIR
import           Data.Hashable
import           Data.IORef
import           Data.Time.Clock
import           Debug.Trace (traceEventIO, traceMarkerIO)

data Hybrid k v = A !(CM.Map k v)
                | AB !(CM.Map k v)
                | B !(PM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

{-# INLINABLE newMap #-}
newMap :: IO (AdaptiveMap k v)
newMap = do
  !m <- CM.empty
  newIORef $ A m

{-# INLINABLE newBMap #-}
-- Temp/Debugging: make it possible to create in the B state.
newBMap :: (Eq k, Hashable k) => IO (AdaptiveMap k v)
newBMap = do
  !m <- PM.newMap
  newIORef $ B m

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get !k !m = do
  state <- readIORef m
  case state of
    A cm  -> CM.lookup k cm
    AB cm -> CM.lookup k cm
    B pm  -> PM.get k pm

{-# INLINABLE size #-}
size :: AdaptiveMap k v -> IO Int
size !m = do
  state <- readIORef m
  case state of
    A cm  -> CM.size cm
    AB cm -> CM.size cm
    B pm  -> PM.size pm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> AdaptiveMap k v -> IO ()
ins !k !v !m = do
  state <- readIORef m
  case state of
    A cm -> CM.insert k v cm
    AB _ -> ins k v m
    B pm -> PM.ins k v pm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> ins k v m)]

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO ()
del !k !m = do
  state <- readIORef m
  case state of
    A cm -> CM.delete k cm
    AB _ -> del k m
    B pm -> PM.del k pm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> del k m)]


transition :: (Eq k, Hashable k) => AdaptiveMap k v -> IO ()
transition m = do
  tik <- readForCAS m
  case peekTicket tik of
    A cm -> do
      (success, _tik) <- casIORef m tik (AB cm)
      -- TODO: if not success, should probably verify that someone else has actually got it into the AB state,
      -- if only for debugging/sanity checking.
      when success $ do

        -- Bad version, delete me after measuring:
        ------------------------------------------
        -- Actually, this one seems like it's hanging with this command [2016.03.05]:
        -- stack bench adaptive-hashmap:bench-adaptive-hashmap --benchmark-arguments="-b adaptive --ops1=12 --ops2=12 --initial=10000 +RTS -s -N12"
        -- CM.freeze cm
        -- l <- CM.unsafeToList cm
        -- pm <- PM.fromList l

        -- The basic algorithm:
        -----------------------
        -- This is about 115ms on 12 thread magramal, with the same command as above.
        -- CM.freeze cm
        -- pm <- PM.newMap
        -- CM.unsafeTraverse_ (\ k v -> PM.ins k v pm) cm

        -- A small optimization.  Single-pass version.
        ------------------------
        -- Huh, this one is about the same time.
        pm <- PM.newMap
        st <- getCurrentTime
        traceMarkerIO "StartFreeze"
        CM.freezeAndTraverse_ (\ k v -> PM.ins k v pm) cm
        traceMarkerIO "EndFreeze"
        en <- getCurrentTime
        putStr $ "(freezeTravTime "++show (diffUTCTime en st)++") "

        let poller = do x <- readIORef m
                        case x of
                          A _  -> return True
                          AB _ -> return True
                          B _  -> return False -- Give up.  Someone else did it.
        -- TODO: pollFreeze version:
        -- if I-am-thread-zero-or-something
        -- then normalFreeze
        -- else pollFreeze poller

        let loop tik =
              do (b,tik2) <- casIORef m tik (B pm)
                 unless b $
                   case peekTicket tik2 of
                     A _  -> error "this is impossible"                          
                     AB _ -> error "transition: should not happen"
                             -- loop tik' -- This should not actually happen, should it?
                     -- Someone else beat us to the punch and that's just fine:
                     B _  -> return ()
        tik <- readForCAS m
        loop tik

    -- Warning, the full lock-free version needs to call freeze/conversion here:
    AB _ -> return ()
    B _ -> return ()

fromList :: (Eq k, Hashable k) => [(k, v)] -> IO (AdaptiveMap k v)
fromList !l = do
  !m <- CM.fromList l
  newIORef $ A m
