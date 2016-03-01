{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.PureMap
       (
         PureMap
       , newMap
       , get
       , ins
       , del
       , snapshot
       )
       where

import Data.Atomics
import Data.IORef
import Data.Hashable
import qualified Data.HashMap.Strict as HM

type PureMap k v = IORef (HM.HashMap k v)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMap k v)
newMap = newIORef HM.empty

snapshot :: PureMap k v -> IO (HM.HashMap k v)
snapshot = readIORef

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do m' <- readIORef m
               return $ HM.lookup k m'

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = loop =<< readForCAS m
  where loop tik = do
          let !m' = peekTicket tik
          (success, t2) <- casIORef m tik $ HM.insert k v m'
          if success then return () else loop t2

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> PureMap k v -> IO ()
del !k !m = loop =<< readForCAS m
  where loop tik = do
          let !m' = peekTicket tik
          (success, t2) <- casIORef m tik $ HM.delete k m'
          if success then return () else loop t2
