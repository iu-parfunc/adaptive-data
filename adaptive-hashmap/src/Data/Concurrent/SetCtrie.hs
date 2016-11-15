{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.SetCtrie (
  Set (..)
  , empty
  , insert
  , delete
  , member
  , transition
  ) where

import Data.Concurrent.Set
import qualified Control.Concurrent.Map as CM
import Control.DeepSeq

newtype Set = SC (CM.Map Int ())

empty :: IO Set
empty = CM.empty >>= (return . SC)

instance DSet Set where
  insert :: Int -> Set -> IO ()
  insert !k (SC !s) = deepseq k $ CM.insert k () s

  delete :: Int -> Set -> IO ()
  delete !k (SC !s) = CM.delete k s

  member :: Int -> Set -> IO Bool
  member !k (SC !s) = do
    !res <- CM.lookup k s
    case res of
      Nothing -> return False
      Just _  -> return True
  
  transition _ = return ()

  output (SC !m) = CM.unsafeToList m >>= putStrLn . show
