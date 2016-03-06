{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Concurrent.PureMapL
       (
         PureMapL
       , newMap
       , get
       , ins
       , del
       )
       where

import qualified Control.Concurrent.Lock as L
import           Data.Hashable
import qualified Data.HashMap.Strict     as HM
import           Data.IORef

type PureMapL k v = IORef (HM.HashMap k v, L.Lock)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMapL k v)
newMap = do
  let !m = HM.empty
  !l <- L.new
  newIORef (m, l)

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMapL k v -> IO (Maybe v)
get !k !r = do
  (m, _) <- readIORef r
  let !res = HM.lookup k m
  return res

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMapL k v -> IO ()
ins !k !v !r =  do
  (m, l) <- readIORef r
  L.acquire l
  writeIORef r (HM.insert k v m, l)
  L.release l
  return ()

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> PureMapL k v -> IO ()
del !k !r = do
  (m, l) <- readIORef r
  L.acquire l
  writeIORef r (HM.delete k m, l)
  L.release l
  return ()
