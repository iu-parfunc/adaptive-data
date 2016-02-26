{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.Adaptive.AdaptiveMap
       (
         AdaptiveMap
       , newMap
       , get
       , ins
       , del
       )
       where

import qualified Control.Concurrent.Adaptive.Ctrie   as CM
import qualified Control.Concurrent.Adaptive.PureMap as PM
import           Control.Exception
import           Data.Atomics
import qualified Data.Concurrent.IORef               as FIR
import           Data.Hashable
import           Data.IORef

data Hybrid k v = A !(PM.PureMap k v)
                | AB !(PM.PureMap k v) !(CM.Map k v)
                | B !(CM.Map k v)
                | BA !(CM.Map k v) !(PM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (AdaptiveMap k v)
newMap = do
  !m <- PM.newMap
  newIORef $ A m

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get !k !m = do
  state <- readIORef m
  case state of
    A pm -> PM.get k pm
    AB pm _ -> PM.get k pm
    B cm -> CM.lookup k cm
    BA cm _ -> CM.lookup k cm
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in get k m),
   Handler (\e -> let _ = (e :: FIR.TransitionException)
                  in do tik <- readForCAS m
                        transition m tik
                        get k m)]

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> AdaptiveMap k v -> IO ()
ins !k !v !m = do
  state <- readIORef m
  case state of
    A pm -> PM.ins k v pm
    AB _ _ -> ins k v m
    B cm -> CM.insert k v cm
    BA _ _ -> ins k v m
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in ins k v m),
   Handler (\e -> let _ = (e :: FIR.TransitionException)
                  in do tik <- readForCAS m
                        transition m tik
                        ins k v m)]

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO ()
del !k !m = do
  state <- readIORef m
  case state of
    A pm -> PM.del k pm
    AB _ _ -> del k m
    B cm -> CM.delete k cm
    BA _ _ -> del k m
  `catches`
  [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                  in del k m),
   Handler (\e -> let _ = (e :: FIR.TransitionException)
                  in do tik <- readForCAS m
                        transition m tik
                        del k m)]

transition :: (Eq k, Hashable k) => AdaptiveMap k v -> Ticket (Hybrid k v) -> IO ()
transition m tick = do
  case peekTicket tick of
    A pm -> do
      cm <- CM.empty
      (success, tick') <- casIORef m tick (AB pm cm)
      if success
        then do let loop tik = do
                      (s, tik') <- casIORef m tik (B cm)
                      if s
                        then return ()
                        else loop tik'
                PM.freeze pm
                l <- PM.toList pm
                mapM_ (\(k, v) -> CM.insert k v cm) l
                loop tick'
        else return ()

    AB _ _ -> return ()
    B cm ->  do
      pm <- PM.newMap
      (success, tick') <- casIORef m tick (BA cm pm)
      if success
        then do let loop tik = do
                      (s, tik') <- casIORef m tik (A pm)
                      if s
                        then return ()
                        else loop tik'
                CM.freeze cm
                l <- CM.unsafeToList cm
                mapM_ (\(k, v) -> PM.ins k v pm) l
                loop tick'
        else return ()
    BA _ _ -> return ()
