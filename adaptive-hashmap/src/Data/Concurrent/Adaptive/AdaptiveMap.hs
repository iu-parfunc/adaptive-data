{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict              #-}
{-# LANGUAGE StrictData          #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Concurrent.Adaptive.AdaptiveMap (
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
