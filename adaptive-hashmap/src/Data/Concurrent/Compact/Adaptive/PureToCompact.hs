{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}

module Data.Concurrent.Compact.Adaptive.PureToCompact where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Atomics
import qualified Data.Concurrent.Compact.PureMap as CM
import qualified Data.Concurrent.IORef           as FIR
import qualified Data.Concurrent.PureMap         as PM
import           Data.Hashable
import           Data.IORef
import           GHC.Conc                        (yield)

data Hybrid k v = A (PM.PureMap k v)
                | AB (PM.PureMap k v)
                | B (CM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

getState :: AdaptiveMap k v -> IO String
getState r = do
  m <- readIORef r
  case m of
    A _  -> return "A"
    AB _ -> return "AB"
    B _  -> return "B"

{-# INLINE newMap #-}
newMap :: (Eq k, Hashable k) => IO (AdaptiveMap k v)
newMap = do
  m <- PM.newMap
  newIORef $ A m

{-# INLINE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get k m = do
  state <- readIORef m
  case state of
    A pm  -> PM.get k pm
    AB pm -> PM.get k pm
    B cm  -> CM.get k cm

{-# INLINE size #-}
size :: AdaptiveMap k v -> IO Int
size m = do
  state <- readIORef m
  case state of
    A pm  -> PM.size pm
    AB pm -> PM.size pm
    B cm  -> CM.size cm

{-# INLINE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> AdaptiveMap k v -> IO ()
ins k v m = do
  state <- readIORef m
  case state of
    A pm -> PM.ins k v pm
    AB _ -> do transition m; ins k v m
    B cm -> CM.ins k v cm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> ins k v m)]

{-# INLINE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> AdaptiveMap k v -> IO ()
del k m = do
  state <- readIORef m
  case state of
    A pm -> PM.del k pm
    AB _ -> do transition m; del k m
    B cm -> CM.del k cm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> del k m)]

{-# INLINABLE transition #-}
transition :: (NFData k, NFData v) => AdaptiveMap k v -> IO ()
transition m = do
  tik <- readForCAS m
  case peekTicket tik of
    A pm -> do
      (success, tik') <- casIORef m tik (AB pm)
      when success $ do
        hm <- PM.snapshot pm
        cm <- CM.fromMap hm
        _ <- casIORef m tik' (B cm)
        return ()
    AB _ -> wait
    B _ -> return ()

  where
    wait = sleepWait
    sleepWait = do
      t <- readIORef m
      case t of
        A _ -> do
          yield
          sleepWait
        AB _ -> do
          yield
          sleepWait
        B _ -> return ()
