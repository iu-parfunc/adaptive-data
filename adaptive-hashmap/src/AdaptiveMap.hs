{-# LANGUAGE BangPatterns #-}
module AdaptiveMap
       (
         AdaptiveMap
       , newMap
       , get
       , ins
       , del
       , transition
       )
       where

import Data.Atomics
import Data.IORef
import Data.Hashable
import Control.Exception
import qualified Data.Concurrent.IORef as FIR
--import qualified Control.Concurrent.PureMap as PM
import qualified Data.Concurrent.Compact.PureMap as PM
import Control.DeepSeq
import qualified Control.Concurrent.Map as CM
import Control.Monad

data Hybrid k v = A !(CM.Map k v)
                | AB !(CM.Map k v) !(PM.PureMap k v)
                | B !(PM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k, NFData k, NFData v) => IO (AdaptiveMap k v)
newMap = do
  !m <- CM.empty
  newIORef $ A m

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get !k !m = do
  state <- readIORef m
  case state of
    A cm -> CM.lookup k cm
    AB cm _ -> CM.lookup k cm
    B pm -> PM.get k pm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> AdaptiveMap k v -> IO ()
ins !k !v !m = do
  state <- readIORef m
  case state of
    A cm -> CM.insert k v cm
    AB _ _ -> do transition m; ins k v m
    B pm -> PM.ins k v pm
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in ins k v m)]

{-# INLINABLE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> AdaptiveMap k v -> IO ()
del !k !m = do
  state <- readIORef m
  case state of
    A cm -> CM.delete k cm
    AB _ _ -> do transition m ; del k m
    B pm -> PM.del k pm
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in del k m)]

transition :: (Eq k, Hashable k, NFData k, NFData v) => AdaptiveMap k v -> IO ()
transition m = do
  tick <- readForCAS m
  case peekTicket tick of
    A cm -> do
      pm <- PM.newMap
      (success, _) <- casIORef m tick (AB cm pm)
      if success
        then do CM.freezeAndTraverse_ (\ k v -> PM.ins k v pm) cm
                casloop pm
        else transition m
    AB _ _ -> return ()
    B _ ->  return ()
  where
    casloop pm = do
      tik <- readForCAS m
      (success, _) <- casIORef m tik (B pm)
      unless success $ casloop pm


