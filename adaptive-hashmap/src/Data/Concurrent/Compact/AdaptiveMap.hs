{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Concurrent.Compact.AdaptiveMap (
    AdaptiveMap,
    newMap,
    get, getState,
    ins,
    del,
    transition,
    fromList,
    size
    ) where

-- Backpack should eliminate silliness like this in the future:
import qualified Data.Concurrent.Compact.PureMap as PM

#include "AdaptiveMapCore.hs"
