{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBctrie (
  Map
  , empty
  , insert
  , delete
  , lookup
  , transition
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString (ByteString)
import Prelude hiding (lookup)

type Map = CM.Map Int ByteString

empty :: IO Map
empty = CM.empty

instance DB Map where
  insert = CM.insert
  delete = CM.delete
  lookup = CM.lookup
  transition = \_ -> return ()
