{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Concurrent.DBctrie (
  Map
  ) where

import Data.Concurrent.DB
import qualified Control.Concurrent.Map as CM
import Data.ByteString (ByteString)

type Map = CM.Map Int ByteString

instance DB Map where
  empty = CM.empty
  insert = CM.insert
  delete = CM.delete
  lookup = CM.lookup
