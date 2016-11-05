{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBgz (
  Map
  , empty
  , insert
  , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString.Lazy (ByteString)
import Codec.Compression.GZip
import Prelude hiding (lookup)
import Control.DeepSeq

newtype Map = DBZ (CM.Map Int ByteString)

empty :: IO Map
empty = CM.empty >>= (return . DBZ)

instance DB Map where  
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v (DBZ !m) = CM.insert k (force $ compress v) m

  delete :: Int -> Map -> IO ()
  delete !k (DBZ !m) = CM.delete k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k !(DBZ m) = do
    !res <- CM.lookup k m
    case res of
      Nothing -> return Nothing
      Just s  -> return $ Just $ force $ decompress s

  transition (DBZ !m) = CM.lookup 1024 m >>= (\_ -> return ())

  output (DBZ !m) = CM.unsafeToList m >>= putStrLn . show
