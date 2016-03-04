{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Compact.PureMap
       ( PureMap
       , newMap
       , fromMap
       , get
       , ins
       , del
       ) where

import           Control.DeepSeq
import           Data.Atomics
import           Data.Compact
import           Data.Concurrent.IORef (spinlock)
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.IORef

type PureMap k v = IORef (Compact (HM.HashMap k v))

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k, NFData k, NFData v) => IO (PureMap k v)
newMap = newCompact 4096 HM.empty >>= newIORef

fromMap :: (Eq k, Hashable k, NFData k, NFData v) => HM.HashMap k v -> IO (PureMap k v)
fromMap !m = do
  let sz = fromIntegral $ HM.size m
      bytes :: Double = 24 * sz + 256 * logBase 16 sz
  c <- newCompact (fromIntegral $ round bytes) m
  newIORef c

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do
  !c <- readIORef m
  let !hm = getCompact c
  return $ HM.lookup k hm

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = loop =<< readForCAS m
  where
    loop = spinlock
             (\tik -> do
                let !c = peekTicket tik
                    !hm = getCompact c
                c' <- appendCompact c $ HM.insert k v hm
                casIORef m tik c')

{-# INLINABLE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> PureMap k v -> IO ()
del !k !m = loop =<< readForCAS m
  where
    loop = spinlock
             (\tik -> do
                let !c = peekTicket tik
                    !hm = getCompact c
                c' <- appendCompact c $ HM.delete k hm
                casIORef m tik c')
