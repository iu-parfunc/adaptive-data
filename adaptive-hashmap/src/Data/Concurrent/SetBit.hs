{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.SetBit (
  Set
  , empty
  , insert
  , delete
  , member
  , transition
  ) where

import Data.Concurrent.Set
import qualified Data.BitSet.Dynamic as BS
import Control.DeepSeq
import Data.IORef
import Data.Atomics

type Set = IORef (BS.BitSet Int)

empty :: IO Set
empty = newIORef $ BS.empty

instance DSet Set where
  insert :: Int -> Set -> IO ()
  insert !k !ref = deepseq k $ loop (\s -> BS.insert k s) ref

  delete :: Int -> Set -> IO ()
  delete !k !ref = deepseq k $ loop (\s -> BS.delete k s) ref

  member :: Int -> Set -> IO Bool
  member !k !ref = do
    s <- readIORef ref
    return $ BS.member k s
  
  transition _ = return ()

  output !ref = readIORef ref >>= putStrLn . show . BS.toList

loop :: (a -> a) -> IORef a -> IO ()
loop fn ref = do
  tick <- readForCAS ref
  (success, _) <- casIORef ref tick $ fn $ peekTicket tick
  if success
    then return ()
    else loop fn ref
