{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBgz (
  Map
  , empty
  , insert
  -- , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString (ByteString)
--import Codec.Compression.GZip
--import Codec.Compression.QuickLZ
import Codec.Compression.LZ4
import Prelude hiding (lookup)
import Control.DeepSeq

newtype Map = DBZ (CM.Map Int ByteString)

empty :: IO Map
empty = CM.empty >>= (return . DBZ)

instance DB Map where  
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v (DBZ !m) =
    let !v' = force $ compress v
    in case v' of
         Just bs -> deepseq bs $ CM.insert k bs m
         Nothing -> undefined

  -- delete :: Int -> Map -> IO ()
  -- delete !k (DBZ !m) = CM.delete k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k !(DBZ m) = do
    !res <- CM.lookup k m
    case res of
      Nothing -> return Nothing
      Just s  -> let !bs = decompress s
                 in case bs of
                   Just bs' -> deepseq bs' $ return $ Just bs'
                   Nothing -> undefined

  transition (DBZ !m) = CM.lookup 1024 m >>= (\_ -> return ())

  output (DBZ !m) = CM.unsafeToList m >>= putStrLn . show
