{-# LANGUAGE BangPatterns #-}
module Data.Concurrent.PureBag
       (
         PureBag
       , newBag
       , add
       , remove
       )
       where

import Data.Atomics
import Data.IORef

type PureBag a = IORef [a]

{-# INLINABLE newBag #-}
newBag :: IO (PureBag a)
newBag = newIORef []

{-# INLINABLE add #-}
add :: PureBag a -> a -> IO ()
add bag x = do
  tick <- readForCAS bag
  let !rst = peekTicket tick
  (success, _) <- casIORef bag tick (x:rst)
  if success then return () else add bag x

{-# INLINABLE remove #-}
remove :: PureBag a -> IO (Maybe a)
remove bag = do
  tick <- readForCAS bag
  case peekTicket tick of
   [] -> return Nothing
   (x:xs) -> do
     (success, _) <- casIORef bag tick xs
     if success then return $! Just x else remove bag

-- remove bag = atomicModifyIORefCAS bag pop
--   where pop [] = ([], Nothing)
--         pop (x:xs) = (xs, Just x)
