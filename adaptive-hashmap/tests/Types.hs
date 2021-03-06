{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Types where

import           Control.Concurrent
import           Control.DeepSeq
import qualified Control.Exception           as E
import           Control.Monad.Reader
import           Data.Int
import           Data.List                   (sort)
import qualified Data.Vector.Unboxed         as VU
import           GHC.Generics
import           GHC.Stats                   (GCStats (..))
import qualified GHC.Stats                   as Stats
import           GHC.Word
import qualified System.Clock                as C
import           System.Console.CmdArgs      (def, help, ignore, (&=))
import qualified System.Console.CmdArgs      as CA
import           System.CPUTime.Rdtsc
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified Data.Concurrent.Adaptive.AdaptiveMap            as AM
import qualified Data.Concurrent.Compact.Adaptive.CtrieToCompact as CCM
import qualified Data.Concurrent.Compact.Adaptive.PureToCompact  as PCM
import qualified Data.Concurrent.Ctrie                           as CM
import qualified Data.Concurrent.PureMap                         as PM
import qualified Data.Concurrent.PureMapL                        as PML

data Flag =
       Flag
         { ops         :: !Int
         , file        :: !String
         , runs        :: !Int
         , warmup      :: !Int
         , iters       :: !Int64
         , bench       :: !String
         , variants    :: ![String]
         , allvariants :: !Bool
         , ratio       :: !Int64
         , minthreads  :: !Int
         , maxthreads  :: !Int
         , doplot      :: !Bool
         , initial     :: !Int
         , range       :: !Int64
         , precompute  :: !Bool
         , export      :: !Bool
         , randomInts  :: VU.Vector Int64
         , randomPairs :: VU.Vector (Int64, Int64)
         , maxsize     :: !Int
         , stepsize    :: !Int
         , seed        :: !Word64
         }
  deriving (Eq, Show, CA.Data, CA.Typeable)

type Bench a = ReaderT Flag IO a

all_variants = ["pure", "ctrie", "adaptive", "cc-adaptive", "pc-adaptive"]

flag :: Flag
flag = Flag
  { ops = 100000 &= help "Total number of operations"
  , warmup = 5 &= help "Number of warmup runs"
  , file = "report" &= help "Report file prefix"
  , iters = 1 &= help "Number of iterations"
  , runs = 50 &= help "Number of runs"
  , bench = "hotcold" &= help "Benchmark {ins, insdel, random, hotcold, hot, cold, transition}"
  , variants = def &= help "Variants {pure, ctrie, adaptive, c-adaptive}"
  , allvariants = False &= help "Use all builtin variants"
  , ratio = 200 &= help "Cold-to-hot ops ratio"
  , maxthreads = 0 &= help "Max number of threads"
  , minthreads = 1 &= help "Min number of threads"
  , doplot = False &= help "Plot the output with gnuplot"
  , initial = 10000 &= help "Initial size"
  , range = 1000000 &= help "Range for random values"
  , precompute = True &= help "Precompute random values"
  , randomInts = VU.empty &= ignore
  , randomPairs = VU.empty &= ignore
  , export = True &= help "Whether to export csv files"
  , maxsize = 1000000000 &= help "Maximum size (used for the transition benchmark)"
  , stepsize = 100000 &= help "Step size (used for the transition benchmark)"
  , seed = 4096 &= help "Seed for RNG"
  }

{-# INLINE rand #-}
rand :: PCG.GenIO -> Int64 -> IO Int64
rand !g !n = PCG.uniformR (0, n) g

-- | Run a function several times and take the median time.
{-# INLINE run #-}
run :: Int -> IO Measured -> IO Measured
run runs fn = do
  let rs = if even runs
             then runs + 1
             else runs
      mid = (1 + rs) `quot` 2
  ms <- for 1 rs $ const fn
  return $! sort ms !! (mid - 1)

data Measured =
       Measured
         { measTime               :: !Double
         , measCpuTime            :: !Double
         , measCycles             :: !Int64
         , measIters              :: !Int64
         , measAllocated          :: !Int64
         , measNumGcs             :: !Int64
         , measBytesCopied        :: !Int64
         , measMutatorWallSeconds :: !Double
         , measMutatorCpuSeconds  :: !Double
         , measGcWallSeconds      :: !Double
         , measGcCpuSeconds       :: !Double
         }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

measured :: Measured
measured = Measured
  { measTime = 0
  , measCpuTime = 0
  , measCycles = 0
  , measIters = 0
  , measAllocated = minBound
  , measNumGcs = minBound
  , measBytesCopied = minBound
  , measMutatorWallSeconds = bad
  , measMutatorCpuSeconds = bad
  , measGcWallSeconds = bad
  , measGcCpuSeconds = bad
  }
  where
    bad = -1 / 0

getTime :: IO Double
getTime = ((1.0e-9 *) . fromInteger . C.toNanoSecs) `fmap` C.getTime C.Monotonic

timeit :: String -> IO a -> IO a
timeit msg act = do
  st <- getTime
  x  <- act
  en <- getTime
  when (msg /= "") $  do
    putStrLn $ msg ++ ", time: " ++ show (en-st)
    hFlush stdout
  return x

getCPUTime :: IO Double
getCPUTime = ((1.0e-9 *) . fromInteger . C.toNanoSecs) `fmap` C.getTime C.ProcessCPUTime

getCycles :: IO Word64
getCycles = rdtsc

getGCStats :: IO (Maybe GCStats)
getGCStats = (Just `fmap` Stats.getGCStats) `E.catch` \(_ :: E.SomeException) -> return Nothing

applyGCStats :: Maybe GCStats
             -> Maybe GCStats
             -> Measured
             -> Measured
applyGCStats (Just end) (Just start) m = m
  { measAllocated = diff bytesAllocated
  , measNumGcs = diff numGcs
  , measBytesCopied = diff bytesCopied
  , measMutatorWallSeconds = diff mutatorWallSeconds
  , measMutatorCpuSeconds = diff mutatorCpuSeconds
  , measGcWallSeconds = diff gcWallSeconds
  , measGcCpuSeconds = diff gcCpuSeconds
  }
  where
    diff f = f end - f start
applyGCStats _ _ m = m

-- | measure `iters` times
{-# INLINE measure #-}
measure :: (MonadIO m, NFData a) => Int64 -> IO a -> m Measured
measure !iters !f = liftIO $ do
  performGC
  startStats <- getGCStats
  startTime <- getTime
  startCpuTime <- getCPUTime
  startCycles <- getCycles
  _ <- for_ 1 iters $ const f
  endTime <- getTime
  endCpuTime <- getCPUTime
  endCycles <- getCycles
  endStats <- getGCStats
  let !m = applyGCStats endStats startStats $ measured
        { measTime = max 0 (endTime - startTime)
        , measCpuTime = max 0 (endCpuTime - startCpuTime)
        , measCycles = max 0 (fromIntegral (endCycles - startCycles))
        , measIters = iters
        }
  return m

-- | measure once
{-# INLINE measureOnce #-}
measureOnce :: (MonadIO m, NFData a) => IO a -> m Measured
measureOnce = measure 1

-- | measure `iters` times and rescale
{-# INLINE measure' #-}
measure' :: NFData a => IO a -> Bench Measured
measure' !f = do
  !iters <- reader iters
  !m <- measure iters f
  return $! rescale m

{-# INLINE rescale #-}
rescale :: Measured -> Measured
rescale m@Measured { .. } = m
  { measTime = d measTime
  , measCpuTime = d measCpuTime
  , measCycles = i measCycles
  -- skip measIters
  , measNumGcs = i measNumGcs
  , measBytesCopied = i measBytesCopied
  , measMutatorWallSeconds = d measMutatorWallSeconds
  , measMutatorCpuSeconds = d measMutatorCpuSeconds
  , measGcWallSeconds = d measGcWallSeconds
  , measGcCpuSeconds = d measGcCpuSeconds
  }
  where
    d k = maybe k (/ iters) (fromDouble k)
    i k = maybe k (round . (/ iters)) (fromIntegral <$> fromInt k)
    iters = fromIntegral measIters :: Double
    fromDouble d
      | isInfinite d || isNaN d = Nothing
      | otherwise = Just d
    fromInt i
      | i == minBound = Nothing
      | otherwise = Just i

{-# INLINABLE forkJoin #-}
forkJoin :: Int -> (Int -> IO a) -> IO [a]
forkJoin num act = loop num []
  where
    loop 0 !ls = mapM takeMVar ls
    loop n !ls = do
      mv <- newEmptyMVar
      _ <- forkOn (n - 1) $ do
             !v <- act (n - 1)
             putMVar mv v
      loop (n - 1) (mv : ls)

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, MonadIO m, NFData a) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop !i
      | i > end = return ()
      | otherwise = fn i >>= liftIO . E.evaluate . rnf >> loop (i + 1)

-- WARNING!! THIS IS NOT TAIL RECURSIVE!
{-# INLINABLE fold #-}
fold :: (Num n, Ord n, Monad m, NFData b) => n -> n -> b -> (b -> a -> b) -> (n -> m a) -> m b
fold start end _ _ _
  | start > end = error "start greater than end"
fold start end !z fld fn = loop start
  where
    loop !i
      | i > end = return z
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          -- Doesn't this deep strictness at each iter make this QUADRATIC!
          return $! fld xs x

for :: (Num n, Ord n, Monad m, NFData a) => n -> n -> (n -> m a) -> m [a]
for start end _
  | start > end = error "start greater than end"
for start end fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          return $! x : xs

{-# INLINABLE fori #-}
fori :: (Num n, Ord n, MonadIO m, NFData n, NFData a) => n -> n -> (n -> m a) -> m [(n, a)]
fori start end _
  | start > end = error "start greater than end"
fori start end fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          return $! (i, x) : xs

{-# INLINABLE fori' #-}
fori' :: (Num n, Ord n, MonadIO m, NFData n, NFData a) => n -> n -> n -> (n -> m a) -> m [(n, a)]
fori' start end _ _
  | start > end = error "start greater than end"
fori' start end step fn = loop start
  where
    loop !i
      | i > end = return []
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + step)
          return $! (i, x) : xs

data GenericImpl m =
  GenericImpl
  { newMap     :: IO m
  , get        :: Int64 -> m -> IO (Maybe Int64)
  , insert     :: Int64 -> Int64 -> m -> IO ()
  , delete     :: Int64 -> m -> IO ()
  , transition :: m -> IO ()
  , size       :: m -> IO Int
  , state      :: m -> IO String
  }

{-# INLINE nop #-}
nop :: Applicative m => a -> m ()
nop _ = pure ()

nopImpl :: GenericImpl ()
nopImpl = GenericImpl
            (return ())
            (\_ _ -> return Nothing)
            (\_ _ _ -> return ())
            (\_ _ -> return ())
            nop
            (\_ -> return 0)
            (\_ -> return "_")

pureImpl :: GenericImpl (PM.PureMap Int64 Int64)
pureImpl = GenericImpl PM.newMap PM.get PM.ins PM.del nop PM.size (\_ -> return "_")

purelImpl :: GenericImpl (PML.PureMapL Int64 Int64)
purelImpl = GenericImpl PML.newMap PML.get PML.ins PML.del nop PML.size (\_ -> return "_")

ctrieImpl :: GenericImpl (CM.Map Int64 Int64)
ctrieImpl = GenericImpl CM.empty CM.lookup CM.insert CM.delete nop CM.size (\_ -> return "_")

adaptiveImpl :: GenericImpl (AM.AdaptiveMap Int64 Int64)
adaptiveImpl = GenericImpl AM.newMap AM.get AM.ins AM.del AM.transition AM.size AM.getState
-- Quick hack below, start in B state
-- ----------------------------------
-- Interestingly this is STILL very different perf wise from pure on
-- the following command, which seems bogus.
--     stack bench adaptive-hashmap:bench-adaptive-hashmap-1 '--benchmark-arguments=--ops=10000000 --bench=hotcold --runs=3 --minthreads=1 --maxthreads=12 --ratio=5000 --variants=adaptive +RTS -N12 -A100M -H4G -qa -s -ls'
-- adaptiveImpl = GenericImpl AM.newBMap AM.get AM.ins AM.del AM.transition AM.size
-- ----------------------------------

ccadaptiveImpl :: GenericImpl (CCM.AdaptiveMap Int64 Int64)
ccadaptiveImpl = GenericImpl CCM.newMap CCM.get CCM.ins CCM.del CCM.transition CCM.size CCM.getState

pcadaptiveImpl :: GenericImpl (PCM.AdaptiveMap Int64 Int64)
pcadaptiveImpl = GenericImpl PCM.newMap PCM.get PCM.ins PCM.del PCM.transition PCM.size PCM.getState
