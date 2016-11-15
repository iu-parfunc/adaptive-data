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

newtype Set = SB (IORef (BS.BitSet Int))

empty :: IO Set
empty = do
  r <- newIORef $ BS.empty
  return $ SB r

instance DSet Set where
  insert :: Int -> Set -> IO ()
  insert !k !(SB ref) = deepseq k $ loop (\s -> BS.insert k s) ref

  delete :: Int -> Set -> IO ()
  delete !k !(SB ref) = deepseq k $ loop (\s -> BS.delete k s) ref

  member :: Int -> Set -> IO Bool
  member !k !(SB ref) = do
    s <- readIORef ref
    return $ BS.member k s
  
  transition _ = return ()

  output !(SB ref) = readIORef ref >>= putStrLn . show . BS.toList

loop :: (a -> a) -> IORef a -> IO ()
loop fn ref = do
  tick <- readForCAS ref
  (success, _) <- casIORef ref tick $ fn $ peekTicket tick
  if success
    then return ()
    else loop fn ref
