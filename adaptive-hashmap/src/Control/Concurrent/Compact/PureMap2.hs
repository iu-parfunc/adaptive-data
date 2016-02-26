{-# LANGUAGE BangPatterns #-}

module Control.Concurrent.Compact.PureMap2
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
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           System.IO.Unsafe

type PureMap k v = Compact (IORef (HM.HashMap k v))

instance NFData a => NFData (IORef a) where
  rnf a = unsafePerformIO $ modifyIORef' a force

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k, NFData k, NFData v) => IO (PureMap k v)
newMap = newIORef HM.empty >>= newCompact 64

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !c = do !hm <- readIORef $ getCompact c
               return $ HM.lookup k hm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> PureMap k v -> IO ()
ins !k !v !c = loop =<< readForCAS io
  where io = getCompact c
        loop tik = do
          let !m = peekTicket tik
          (success, t2) <- casIORef io tik $ HM.insert k v m
          if success then return () else loop t2

{-# INLINABLE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> PureMap k v -> IO ()
del !k !c = loop =<< readForCAS io
  where io = getCompact c
        loop tik = do
          let !m = peekTicket tik
          (success, t2) <- casIORef io tik $ HM.delete k m
          if success then return () else loop t2
