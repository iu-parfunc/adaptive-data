{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Adaptive.AdaptiveMap
       (
         AdaptiveMap
       , newMap
       , get
       , ins
       , del
       , transition
       )
       where

import qualified Control.Concurrent.Adaptive.Ctrie   as CM
import qualified Control.Concurrent.PureMap          as PM
import           Control.Exception
import           Data.Atomics
import qualified Data.Concurrent.IORef               as FIR
import           Data.Hashable
import           Data.IORef

data Hybrid k v = A !(CM.Map k v)
                | AB !(CM.Map k v)
                | B !(PM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (AdaptiveMap k v)
newMap = do
  !m <- CM.empty
  newIORef $ A m

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get !k !m = do
  state <- readIORef m
  case state of
    A cm -> CM.lookup k cm
    AB cm -> CM.lookup k cm
    B pm -> PM.get k pm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> AdaptiveMap k v -> IO ()
ins !k !v !m = do
  state <- readIORef m
  case state of
    A cm -> CM.insert k v cm
    AB _ -> ins k v m
    B pm -> PM.ins k v pm
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in ins k m)]

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO ()
del !k !m = do
  state <- readIORef m
  case state of
    A cm -> CM.delete k cm
    AB _ -> del k m
    B pm -> PM.del k pm
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in del k m)]

transition :: (Eq k, Hashable k) => AdaptiveMap k v -> IO ()
transition m = do
  tick <- readForCAS m
  case peekTicket tick of
    A cm ->  do
      pm <- PM.newMap
      (success, tick') <- casIORef m tick (AB cm)
      if success
        then do let loop tik = do
                      (s, tik') <- casIORef m tik (B pm)
                      if s
                        then return ()
                        else loop tik'
                CM.freeze cm
                l <- CM.unsafeToList cm
                mapM_ (\(k, v) -> PM.ins k v pm) l
                loop tick'
        else return ()
    AB _ -> return ()
    B _ -> return ()
