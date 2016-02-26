{-# LANGUAGE BangPatterns #-}

module Control.Concurrent.Compact.PureMap
       ( PureMap
       , newMap
       , get
       , ins
       , del
       ) where

import           Control.DeepSeq
import           Data.Atomics
import           Data.Compact
import           Data.Hashable
import qualified Data.HashMap.Lazy as HM
import           Data.IORef

type PureMap k v = IORef (Compact (HM.HashMap k v))

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k, NFData k, NFData v) => IO (PureMap k v)
newMap = newCompact 64 HM.empty >>= newIORef

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do !c <- readIORef m
               let !hm = getCompact c
               return $ HM.lookup k hm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = loop =<< readForCAS m
  where loop tik = do
          let !c = peekTicket tik
              !hm = getCompact c
          c' <- appendCompact c $ HM.insert k v hm
          (success, t2) <- casIORef m tik c'
          if success then return () else loop t2

{-# INLINABLE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> PureMap k v -> IO ()
del !k !m = loop =<< readForCAS m
  where loop tik = do
          let !c = peekTicket tik
              !hm = getCompact c
          c' <- appendCompact c $ HM.delete k hm
          (success, t2) <- casIORef m tik c'
          if success then return () else loop t2
