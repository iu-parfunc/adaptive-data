{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

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
