{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBagz (
  Map
  , empty
  , insert
  -- , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Data.Concurrent.DBctrie as DBC
import qualified Data.Concurrent.DBgz as DBZ
import Data.ByteString (ByteString)
import Prelude hiding (lookup)
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Atomics
import System.Mem
import System.IO
import qualified Data.Concurrent.IORef as FIR
import qualified Control.Concurrent.Map as CM

data Hybrid = A !DBC.Map
            | AB !DBC.Map !DBZ.Map
            | B !DBZ.Map

type Map = IORef Hybrid

empty :: IO Map
empty = do
  m <- DBC.empty
  newIORef $ A m

instance DB Map where
  insert :: Int -> ByteString -> Map -> IO ()
  insert !k !v !ref = do
    state <- readIORef ref
    case state of
      A cm -> deepseq v $ insert k v cm
      AB _ _ -> do transition ref; insert k v ref
      B zm -> deepseq v $ insert k v zm
    `catches`
    [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                    in insert k v ref)]

  -- delete :: Int -> Map -> IO ()
  -- delete !k (DBP !m) = PM.del k m

  lookup :: Int -> Map -> IO (Maybe ByteString)
  lookup !k !ref = do
    state <- readIORef ref
    case state of
      A cm -> ret cm
      AB cm _ -> ret cm
      B zm -> ret zm
    where
      ret m = do
        !res <- lookup k m
        case res of
          Nothing -> return Nothing
          Just s  -> deepseq s $ return $ Just s
{-
  transition !ref = do
    putStrLn $ "transition"
    hFlush stdout
    return ()
-}
  transition !ref = do
    tick <- readForCAS ref
    case peekTicket tick of
      A dbc@(DBC.DBC cm) -> do
        zm <- DBZ.empty
        (success, _) <- casIORef ref tick (AB dbc zm)
        if success
          then do putStrLn $ "transition"
                  hFlush stdout
                  CM.freezeAndTraverse_ (\k v -> do
                                            insert k v zm) cm
                  casloop zm
          else transition ref
      AB _ _ -> return ()
      B _ ->  return ()
    where
      casloop m = do
        tik <- readForCAS ref
        (success, _) <- casIORef ref tik (B m)
        unless success $ casloop m

  output !_ = undefined

