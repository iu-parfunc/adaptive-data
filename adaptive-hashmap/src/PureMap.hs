{-# LANGUAGE BangPatterns #-}
module PureMap
       (
         PureMap
       , newMap
       , get
       , ins
       , del
       , toList
       , freeze
       )
       where

import Data.Hashable
import Control.Exception
import EntryRef
import qualified Data.HashMap.Strict as HM

type PureMap k v = EntryRef (HM.HashMap k v)

threshold :: Int
threshold = 2

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMap k v)
newMap = newEntryRef HM.empty

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do m' <- readEntryRef m
               return $ HM.lookup k m'

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = do
  tik <- readEntryRefForCAS m
  loop tik threshold
  where loop !_   !i | i == 0 = throwIO TransitionException
        loop !tik !i = do
          let !m' = peekEntryRefTicket tik
          (success, t2) <- casEntryRef m tik $ HM.insert k v m'
          if success then return () else loop t2 $ i - 1

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> PureMap k v -> IO ()
del !k !m = do
  tik <- readEntryRefForCAS m
  loop tik threshold
  where loop !_   !i | i == 0 = throwIO TransitionException
        loop !tik !i = do
          let !m' = peekEntryRefTicket tik
          (success, t2) <- casEntryRef m tik $ HM.delete k m'
          if success then return () else loop t2 $ i - 1

{-# INLINABLE toList #-}
toList :: (Eq k, Hashable k) => PureMap k v -> IO ([(k, v)])
toList !m = do
  !m' <- readEntryRef m
  return $ HM.toList m'

{-# INLINABLE freeze #-}
freeze :: (Eq k, Hashable k) => PureMap k v -> IO ()
freeze !m = loop =<< readEntryRefForCAS m
  where loop !tik = do
          (success, tik') <- freezeEntryRef m tik
          if success then return() else loop tik'
