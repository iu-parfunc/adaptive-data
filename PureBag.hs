{-# LANGUAGE BangPatterns #-}
module PureBag
       (
         PureBag
       , newBag
       , add
       , remove
       )
       where

import Data.Atomics
import Data.IORef
import EntryRef

type PureBag a = EntryRef [a]

{-# INLINABLE newBag #-}
newBag :: IO (PureBag a)
newBag = newEntryRef []

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO (Maybe a)
add bag !x =   loop =<< readForCAS bag 
 where
 loop tik = do
   let !rst = peekTicket tik
     in case rst of
     Val rst -> 
       do (success, t2) <- casIORef bag tik $ Val (x:rst) ;
              if success
              then return (Just x)
              else return Nothing
     Copied _ -> return Nothing

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = loop =<< readForCAS bag
 where
 loop tik = 
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
