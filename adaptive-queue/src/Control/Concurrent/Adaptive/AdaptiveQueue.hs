{-# LANGUAGE BangPatterns #-}

module Control.Concurrent.Adaptive.AdaptiveQueue where

import qualified Control.Concurrent.Adaptive.MichaelScottQueue as MSQ
import qualified Control.Concurrent.PureQueue                  as PQ
import           Data.IORef

data Hybrid a = A !(MSQ.LinkedQueue a)
              | AB !(MSQ.LinkedQueue a) !(PQ.PureQueue a)
              | B !(PQ.PureQueue a)
              | BA !(PQ.PureQueue a) !(MSQ.LinkedQueue a)

type AdaptiveQueue a = IORef (Hybrid a)
