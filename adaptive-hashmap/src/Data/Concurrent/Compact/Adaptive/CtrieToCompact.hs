{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}

module Data.Concurrent.Compact.Adaptive.CtrieToCompact (
    AdaptiveMap,
    newMap, newBMap,
    get, getState,
    ins,
    del,
    transition,
    fromList,
    size
    ) where

-- These implementations differ only in the puremap implementation:
import qualified Data.Concurrent.PureMap as PM

#include "AdaptiveMapCore.hs"
