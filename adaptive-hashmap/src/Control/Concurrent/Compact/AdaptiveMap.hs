{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Control.Concurrent.Compact.AdaptiveMap (
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
import qualified Control.Concurrent.Compact.PureMap as PM

#include "AdaptiveMapCore.hs"


