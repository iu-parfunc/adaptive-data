{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Control.Concurrent.Adaptive.AdaptiveMap (
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
import qualified Control.Concurrent.PureMap        as PM

#include "AdaptiveMapCore.hs"
