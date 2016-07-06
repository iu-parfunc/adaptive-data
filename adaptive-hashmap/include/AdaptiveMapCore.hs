-- WARNING: This is not a complete module, but a fragment to be #included

import qualified Data.Concurrent.Ctrie as CM
import qualified Data.HashMap.Strict   as HM
import           Control.Exception
import           Control.Monad
import           Data.Atomics
import qualified Data.Concurrent.IORef             as FIR
import           Data.Hashable
import           Data.IORef
import           Data.Time.Clock
import           Debug.Trace (traceEventIO, traceMarkerIO)
import           Control.DeepSeq
import           GHC.Conc (yield)
import           System.IO.Unsafe (unsafePerformIO)

-- import qualified Data.Map as M
import Data.TLS.PThread

data Hybrid k v = A (CM.Map k v)
                | AB (CM.Map k v)
                | B (PM.PureMap k v)

type AdaptiveMap k v = IORef (Hybrid k v)

getState :: AdaptiveMap k v -> IO String
getState r =
  do m <- readIORef r
     case m of
       A _  -> return "A"
       AB _ -> return "AB"
       B _  -> return "B"

{-# INLINE newMap #-}
newMap :: IO (AdaptiveMap k v)
newMap = do
  m <- CM.empty
  newIORef $ A m

{-# INLINE newBMap #-}
-- Temp/Debugging: make it possible to create in the B state.
newBMap :: (Eq k, Hashable k, NFData k, NFData v) => IO (AdaptiveMap k v)
newBMap = do
  m <- PM.newMap
  newIORef $ B m

{-# INLINE get #-}
get :: (Eq k, Hashable k) => k -> AdaptiveMap k v -> IO (Maybe v)
get k m = do
  state <- readIORef m
  case state of
    A cm  -> CM.lookup k cm
    AB cm -> CM.lookup k cm
    B pm  -> PM.get k pm

{-# INLINE size #-}
size :: AdaptiveMap k v -> IO Int
size m = do
  state <- readIORef m
  case state of
    A cm  -> CM.size cm
    AB cm -> CM.size cm
    B pm  -> PM.size pm

{-# INLINE ins #-}
ins :: (Eq k, Hashable k, NFData k, NFData v) => k -> v -> AdaptiveMap k v -> IO ()
ins k v m = do
  state <- readIORef m
  case state of
    A cm -> CM.insert k v cm
    AB _ -> do transition m; ins k v m
    B pm -> PM.ins k v pm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> ins k v m)]

{-# INLINE del #-}
del :: (Eq k, Hashable k, NFData k, NFData v) => k -> AdaptiveMap k v -> IO ()
del k m = do
  state <- readIORef m
  case state of
    A cm -> CM.delete k cm
    AB _ -> do transition m; del k m
    B pm -> PM.del k pm
  `catches`
  [Handler (\(_ :: FIR.CASIORefException) -> del k m)]

{-# INLINABLE transition #-}
transition :: (Eq k, Hashable k, NFData k, NFData v) => AdaptiveMap k v -> IO ()
transition m = do
  tik <- readForCAS m
  case peekTicket tik of
    A cm -> do
      (success, _tik) <- casIORef m tik (AB cm)
      -- TODO: if not success, should probably verify that someone else has actually got
      -- it into the AB state, if only for debugging/sanity checking.
      if success then gofreeze True cm
                 else wait cm

    -- Warning, the full lock-free version needs to call freeze/conversion here:
    AB cm -> wait cm
    B  _  -> return ()
 where
  -- Option (0) just let transition return even if we're not in B state:
  -- wait _ = return ()
  -- Option (1), spin until we reach the B state.  NOT LOCK FREE!  
  wait _ = sleepWait
  -- Option (2): block on an MVar.  TODO!
  -- Option (3): Help by also calling freeze!
  -- wait = gofreeze False

  sleepWait =
   do t <- readIORef m
      case t of
        A  _ -> do yield; sleepWait
        AB _ -> do yield; sleepWait
        B  _ -> return ()

  -- We only measure time on the first thread to initiate:
  gofreeze False cm = helper cm
  gofreeze True cm = do
      st <- getCurrentTime
      traceMarkerIO "StartFreeze"
      helper cm
      traceMarkerIO "EndFreeze"
      en <- getCurrentTime
      -- This would be better as an event trace or something that doesn't mangle parallel output:
      putStrLn $ "(freezeTravTime "++show (diffUTCTime en st)++") "

  helper cm = 
   do
      -- The basic algorithm:
      -----------------------
      -- This is about 115ms on 12 thread magramal, with the same command as above.
      -- CM.freeze cm
      -- pm <- PM.newMap
      -- CM.unsafeTraverse_ (\ k v -> PM.ins k v pm) cm

      -- A small optimization.  Single-pass version.
      ------------------------
      -- -- Huh, this one is about the same time.
      -- -- pm <- PM.newMap
      -- -- CM.freezeAndTraverse_ (\ k v -> PM.ins k v pm) cm
      -- hm <- CM.freezeFold (\h k v -> HM.insert k v h) HM.empty cm
      -- pm <- PM.fromMap hm -- TODO: use fromMapSized

      -- Unfinished: polling, assymetric version:
      -------------------------
      let poller = do x <- readIORef m
                      case x of
                        A _  -> return True
                        AB _ -> return True
                        B _  -> return False -- Give up.  Someone else did it.
      -- TODO: pollFreeze version:
      -- if I-am-thread-zero-or-something
      -- then normalFreeze
      -- else pollFreeze poller

      -- [2016.07.06] A new, helping-based lock-free version:
      -------------------------------------------------------
      
      perms <- getTLS myPerms
      acc   <- newIORef HM.empty
      CM.freezeRandConvert perms cm acc
      hm <- readIORef acc
      pm <- PM.fromMap hm -- TODO: use fromMapSized

      --------------------------
      let install tik =
            do (b,tik2) <- casIORef m tik (B pm)
               unless b $
                 case peekTicket tik2 of
                   A _  -> error "this is impossible"
                   AB _ -> error "transition: should not happen"
                           -- install tik' -- This should not actually happen, should it?
                   -- Someone else beat us to the punch and that's just fine:
                   B _  -> return ()
      tik <- readForCAS m
      install tik


{-# INLINE fromList #-}
fromList :: (Eq k, Hashable k) => [(k, v)] -> IO (AdaptiveMap k v)
fromList l = do
  m <- CM.fromList l
  newIORef $ A m

-- Thread-local permutations:
{-# NOINLINE myPerms #-}
myPerms :: TLS CM.Perms
myPerms = unsafePerformIO $
          mkTLS (do putStrLn "YAY, making perms!"
                    CM.makePerms)

-- permTable :: IORef (M.Map ThreadId CM.Perms)

{-
type AllPerms = V.Vector CM.Perms

-- | Build the perms used by all threads.
mkAllPerms :: IO AllPerms
mkAllPerms = do 
  perms <- V.generateM threads (\_ -> CM.makePerms)
  evaluate (rnf perms)
  return perms
-}
