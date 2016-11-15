{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.SetAdaptive (
  Set
  , empty
  , insert
  , delete
  , member
  , transition
  ) where

import Data.Concurrent.Set
import qualified Data.Concurrent.SetCtrie as SC
import qualified Data.Concurrent.SetBit as SB
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Atomics
import qualified Data.Concurrent.IORef as FIR
import qualified Control.Concurrent.Map as CM

data Hybrid = A !SC.Set
            | AB !SC.Set !SB.Set
            | B !SB.Set

type Set = IORef Hybrid

empty :: IO Set
empty = do
  m <- SC.empty
  newIORef $ A m

instance DSet Set where
  insert :: Int -> Set -> IO ()
  insert !k !ref = do
    state <- readIORef ref
    case state of
      A sc -> insert k sc
      AB _ _ -> insert k ref
      B sb -> insert k sb
    `catches`
    [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                    in insert k ref)]

  delete :: Int -> Set -> IO ()
  delete !k !ref = do
    state <- readIORef ref
    case state of
      A sc -> delete k sc
      AB _ _ -> delete k ref
      B sb -> delete k sb
    `catches`
    [Handler (\e -> let _ = (e :: FIR.CASIORefException)
                    in delete k ref)]

  member :: Int -> Set -> IO Bool
  member !k !ref = do
    state <- readIORef ref
    case state of
      A sc -> ret sc
      AB sc _ -> ret sc
      B sb -> ret sb
    where
      ret s = member k s

  transition !ref = do
    tick <- readForCAS ref
    case peekTicket tick of
      A sc@(SC.SC ctrie) -> do
        sb <- SB.empty
        (success, _) <- casIORef ref tick (AB sc sb)
        if success
          then do CM.freezeAndTraverse_ (\k _ -> do
                                            insert k sb) ctrie
                  casloop sb
          else transition ref
      AB _ _ -> return ()
      B _ ->  return ()
    where
      casloop s = do
        tik <- readForCAS ref
        (success, _) <- casIORef ref tik (B s)
        unless success $ casloop s

  output !_ = undefined

