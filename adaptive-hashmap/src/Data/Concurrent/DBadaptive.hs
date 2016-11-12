{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBadaptive (
  Map
  , empty
  , insert
  -- , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified AdaptiveMap as AM
import Data.ByteString (ByteString)
import Prelude hiding (lookup)
import Control.DeepSeq

newtype Map = DBA (AM.AdaptiveMap Int ByteString)

empty :: IO Map
empty = AM.newMap >>= (return . DBA)

instance DB Map where
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v (DBA !m) = deepseq v $ AM.ins k v m

  -- delete :: Int -> Map -> IO ()
  -- delete !k (DBP !m) = PM.del k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k (DBA !m) = do
    !res <- AM.get k m
    case res of
      Nothing -> return Nothing
      Just s  -> deepseq s $ return $ Just s
  
  transition (DBA !m) = AM.transition m

  output (DBA !_) = undefined
