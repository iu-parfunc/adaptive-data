{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBpure (
  Map
  , empty
  , insert
  -- , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.PureMap as PM
import Data.ByteString (ByteString)
import Prelude hiding (lookup)
import Control.DeepSeq

newtype Map = DBP (PM.PureMap Int ByteString)

empty :: IO Map
empty = PM.newMap >>= (return . DBP)

instance DB Map where
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v (DBP !m) = deepseq v $ PM.ins k v m

  -- delete :: Int -> Map -> IO ()
  -- delete !k (DBP !m) = PM.del k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k (DBP !m) = do
    !res <- PM.get k m
    case res of
      Nothing -> return Nothing
      Just s  -> deepseq s $ return $ Just s
  
  transition (DBP !m) = PM.get 1024 m >>= (\_ -> return ())

  output (DBP !_) = undefined
