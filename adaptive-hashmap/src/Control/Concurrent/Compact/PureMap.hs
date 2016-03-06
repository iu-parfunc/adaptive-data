{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.Compact.PureMap
       ( PureMap
       , newMap
       , get
       , ins
       , del
       , fromList
       , toList
       , fromMap, fromMapSized
       , size
       ) where

import           Control.DeepSeq
import           Data.Atomics
import           Data.Compact
import           Data.Concurrent.IORef (spinlock)
import           Data.Hashable
import qualified Data.HashMap.Strict   as HM
import           Data.IORef
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)

type PureMap k v = IORef (Compact (HM.HashMap k v))

{-# INLINABLE newMap #-}
newMap :: (NFData k, NFData v) => IO (PureMap k v)
newMap = newCompact 4096 HM.empty >>= newIORef

{-# INLINABLE get #-}
get :: (Eq k, Hashable k) => k -> PureMap k v -> IO (Maybe v)
get !k !m = do
  c <- readIORef m
  return $! HM.lookup k $! getCompact c

size :: PureMap k v -> IO Int
size r =
  do c <- readIORef r
     let hm = getCompact c
     return $! HM.size hm


{-# INLINABLE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> PureMap k v -> IO ()
ins !k !v !m =
   do t <- readForCAS m
      error "insert called on a Compact HashMap - defining this as error for now"
      -- putStrLn "WARNING: doing insert on Compact Hashmap!"; hFlush stdout
      -- hPutStr stderr "!"

      -- One alternative here is to delay the compaction.  Point into the Compact,
      -- but don't append until later.

      -- Here's a temporary version.  It's one way to do a locking version:
      atomicModifyIORef' m $
        (\ c -> unsafePerformIO $ 
                do c' <- appendCompact c $! HM.insert k v $! getCompact c
                   return (c',()))
      -- loop t
  where
    -- This is pretty terrible.  Better to make this version just locking.
    loop = spinlock
             (\tik -> do
                let !c = peekTicket tik
                c' <- appendCompact c $! HM.insert k v $! getCompact c
                casIORef m tik c')


{-# INLINABLE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> PureMap k v -> IO ()
del !k !m =
   do t <- readForCAS m
      error "delete called on a Compact HashMap - defining this as error for now"
      loop t
  where
    loop = spinlock
             (\tik -> do
                let !c = peekTicket tik
                    !hm = getCompact c
                c' <- appendCompact c $ HM.delete k hm
                casIORef m tik c')

-- | Approximate byte size of resulting structure based on number of elements.
--  FIXME: This could easily be tuned by looking at the ACTUAL byte sizes.
approxSize :: Int -> Word
approxSize sz = fromIntegral dbl 
  where
  dbl :: Word
  dbl = round $ 24 * szd + 256 * logBase 16 szd
  szd :: Double
  szd = fromIntegral sz

{-# INLINABLE fromList #-}
fromList :: (Eq k, Hashable k, NFData k, NFData v) => [(k, v)] -> IO (PureMap k v)
fromList l = do
  let hm = HM.fromList l
  !c <- newCompact (approxSize (HM.size hm)) hm
  newIORef c

{-# INLINABLE toList #-}
toList :: PureMap k v -> IO [(k, v)]
toList = ((HM.toList . getCompact) `fmap`) . readIORef

-- | Create a compacted map from a map in the regular Haskell heap.
fromMap :: (NFData k, NFData v)
        => HM.HashMap k v -> IO (PureMap k v)
fromMap hm =
  -- FIXME: I doubt it is worth donig an extra traversal here for the size!
  do !c <- newCompact (approxSize (HM.size hm)) hm
     newIORef c

fromMapSized :: (NFData k, NFData v)
        => HM.HashMap k v -> Int -> IO (PureMap k v)
fromMapSized hm sz =
  -- FIXME: I doubt it is worth donig an extra traversal here for the size!
  do !c <- newCompact (approxSize sz) hm
     newIORef c

