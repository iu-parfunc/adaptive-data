{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBgz (
  Map
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Codec.Compression.GZip

type Map = CM.Map Int ByteString

instance DB Map where
  empty :: IO Map
  empty = CM.empty
  
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v !m = CM.insert k (toStrict $ compress $ fromStrict v) m

  delete :: Int -> Map -> IO ()
  delete = CM.delete

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k !m = do
    !res <- CM.lookup k m
    case res of
      Nothing -> return Nothing
      Just s  -> return $ Just $ toStrict $ decompress $ fromStrict s
