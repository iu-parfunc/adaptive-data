{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Concurrent.PureMap
       (
         PureMap
       , newMap
       , get
       , ins
       , del
       , fromList
       , fromMap
       , toList
       , snapshot
       , size
       )
       where

import           Data.Atomics
import           Data.Concurrent.IORef (spinlock)
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.IORef

type PureMap k v = IORef (HM.HashMap k v)

{-# INLINE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMap k v)
newMap = newIORef HM.empty

{-# INLINE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get k m = do
  m' <- readIORef m
  return $ HM.lookup k m'

{-# INLINE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMap k v -> IO ()
ins k v m = loop =<< readForCAS m
  where
    loop = spinlock (\tik -> let m' = peekTicket tik
                             in casIORef m tik $ HM.insert k v m')

{-# INLINE del #-}
del :: (Eq k, Hashable k) => k -> PureMap k v -> IO ()
del k m = loop =<< readForCAS m
  where
    loop = spinlock (\tik -> let m' = peekTicket tik
                             in casIORef m tik $ HM.delete k m')

{-# INLINE fromList #-}
fromList :: (Eq k, Hashable k) => [(k, v)] -> IO (PureMap k v)
fromList = newIORef . HM.fromList

-- | O(1)
{-# INLINE fromMap #-}
fromMap :: HM.HashMap k v -> IO (PureMap k v)
fromMap = newIORef

{-# INLINE toList #-}
toList :: PureMap k v -> IO [(k, v)]
toList = (HM.toList `fmap`) . readIORef

-- | O(1) snapshot
{-# INLINE snapshot #-}
snapshot :: PureMap k v -> IO (HM.HashMap k v)
snapshot = readIORef

{-# INLINE size #-}
size :: PureMap k v -> IO Int
size r = HM.size <$> readIORef r
