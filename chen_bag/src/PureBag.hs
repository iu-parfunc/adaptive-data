{-# LANGUAGE BangPatterns #-}
module PureBag
       (
         PureBag
       , newPureBag
       , add
       , remove
       , empty
       , transition
       )
       where

import Data.Atomics
-- import Data.IORef
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
    Val (_:_) -> return False
    Copied (_:_) -> return False
    _ -> return True

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO (Bool)
add bag !x = do
  tik <- readForCAS bag
  case peekTicket tik of
    Val rst -> do
      (success, _) <- casIORef bag tik $ Val (x:rst)
      return success
    Copied _ ->
      return False

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = do
  tik <- readForCAS bag
  case peekTicket tik of
    Val rst ->
      case rst of
      [] -> return Nothing
      (x:xs) -> do
        (success, _) <- casIORef bag tik $ Val xs
        if success
          then return (Just x)
          else return Nothing
    Copied _ -> return Nothing

transition :: PureBag a -> (EntryVal [a] -> (Bool, EntryVal [a])) -> IO ()
transition bag f = do
    tik <- readForCAS bag
    let (copy, newVal) = f $ peekTicket tik
    if copy
      then do (success, _) <- casIORef bag tik newVal
              if success
                then return ()
                else transition bag f
      else return ()
