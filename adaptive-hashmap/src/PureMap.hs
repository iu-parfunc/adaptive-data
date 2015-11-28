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

{-# INLINABLE newMap #-}
newMap :: (Eq k, Hashable k) => IO (PureMap k v)
newMap = newEntryRef HM.empty

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do m' <- readEntryRef m
               return $ HM.lookup k m'

{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k) => k -> v -> PureMap k v -> IO ()
ins !k !v !m = loop =<< readEntryRefForCAS m
  where loop !tik = do
          let !m' = peekEntryRefTicket tik
          (success, _) <- casEntryRef m tik $ HM.insert k v m'
          if success then return () else throwIO TransitionException

{-# INLINABLE del #-}
del :: (Eq k, Hashable k) => k -> PureMap k v -> IO ()
del !k !m = loop =<< readEntryRefForCAS m
  where loop !tik = do
          let !m' = peekEntryRefTicket tik
          (success, _) <- casEntryRef m tik $ HM.delete k m'
          if success then return () else throwIO TransitionException

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
