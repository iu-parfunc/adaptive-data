{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBctrie (
  Map (..)
  , empty
  , insert
  -- , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString (ByteString)
import Prelude hiding (lookup)
import Control.DeepSeq

newtype Map = DBC (CM.Map Int ByteString)

empty :: IO Map
empty = CM.empty >>= (return . DBC)

instance DB Map where
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v (DBC !m) = deepseq v $ CM.insert k v m

  -- delete :: Int -> Map -> IO ()
  -- delete !k (DBC !m) = CM.delete k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k (DBC !m) = do
    !res <- CM.lookup k m
    case res of
      Nothing -> return Nothing
      Just s  -> deepseq s $ return $ Just s
  
  transition (DBC !m) = CM.lookup 1024 m >>= (\_ -> return ())

  output (DBC !m) = CM.unsafeToList m >>= putStrLn . show
