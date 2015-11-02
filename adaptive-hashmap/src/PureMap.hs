{-# LANGUAGE BangPatterns #-}
module PureMap
       (
         PureMap
       , newMap
       , get
       , ins
       , del
       )
       where

import Data.Atomics
import Data.IORef
import qualified Data.HashMap.Strict as HM

type PureMap k v = IORef (HM.HashMap k v)

{-# INLINABLE newMap #-}
newMap :: IO (PureMap k v)
newMap = undefined

{-# INLINABLE get #-}
get :: k -> PureMap k v -> IO (Maybe v)
get = undefined

{-# INLINABLE ins #-}
ins :: k -> v -> PureMap k v -> IO ()
ins = undefined

{-# INLINABLE del #-}
del :: k -> PureMap k v -> IO ()
del = undefined

