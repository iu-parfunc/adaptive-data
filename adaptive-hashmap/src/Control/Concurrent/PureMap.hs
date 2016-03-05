{-# LANGUAGE BangPatterns #-}
module Control.Concurrent.PureMap
       (
         PureMap
       , newMap
       , get
       , ins
       , del
       , fromList
       , toList
       , snapshot
       )
       where

import           Data.Atomics
import           Data.Concurrent.IORef (spinlock)
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.IORef

type PureMap k v = IORef (HM.HashMap k v)

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMap k v)
newMap = newIORef HM.empty

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do
  m' <- readIORef m
  return $ HM.lookup k m'

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = loop =<< readForCAS m
  where
    loop = spinlock (\tik -> let !m' = peekTicket tik
                             in casIORef m tik $ HM.insert k v m')

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> PureMap k v -> IO ()
del !k !m = loop =<< readForCAS m
  where
    loop = spinlock (\tik -> let !m' = peekTicket tik
                             in casIORef m tik $ HM.delete k m')

{-# INLINABLE fromList #-}
fromList :: (Eq k, Hashable k) => [(k, v)] -> IO (PureMap k v)
fromList = newIORef . HM.fromList

{-# INLINABLE toList #-}
toList :: PureMap k v -> IO [(k, v)]
toList = (HM.toList `fmap`) . readIORef

-- | O(1) snapshot
snapshot :: PureMap k v -> IO (HM.HashMap k v)
snapshot = readIORef
