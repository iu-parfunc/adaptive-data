{-# LANGUAGE BangPatterns #-}
module PureBag
       (
         PureBag
       , newPureBag
       , add
       , remove
       , empty
       )
       where

import Data.Atomics
import Data.IORef
import EntryRef

type PureBag a = EntryRef [a]

{-# INLINABLE newPureBag #-}
newPureBag :: IO (PureBag a)
newPureBag = newEntryRef []

{-# INLINABLE empty #-}
empty :: PureBag a -> IO (Bool)
empty bag = do
  tik <- readForCAS bag
  case peekTicket tik of
    Val (x:xs) -> return False
    Copied (x:xs) -> return False
    _ -> return True

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO (Maybe a)
add bag !x = do
  tik <- readForCAS bag
  case peekTicket tik of
    Val rst -> 
      do (success, t2) <- casIORef bag tik $ Val (x:rst) ;
         if success
           then return (Just x)
           else return Nothing
    Copied _ -> return Nothing

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = do
  tik <- readForCAS bag
  case peekTicket tik of
    Val rst ->
      case rst of
      [] -> return Nothing
      (x:xs) -> do
        (success, t2) <- casIORef bag tik $ Val xs
        if success
          then return (Just x)
          else return Nothing
    Copied _ -> return Nothing
