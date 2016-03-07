{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | This is a second (modified) version of the benchmark harness by Vikraman

module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import qualified Control.Exception           as E
import           Control.Monad
import           Control.Monad.Reader
import           Data.Int
import           Data.List                   (sort)
import           Data.Monoid
import qualified Data.Vector.Unboxed         as VU
import           GHC.Stats                   (GCStats (..))
import qualified GHC.Stats                   as Stats
import           GHC.Word
import qualified System.Clock                as C
import           System.Console.CmdArgs      (help, ignore, (&=))
import qualified System.Console.CmdArgs      as CA
import           System.CPUTime.Rdtsc
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

import           Graphics.Gnuplot.Simple       (Attribute (..),
                                                LineAttr (LineTitle),
                                                LineSpec (CustomStyle),
                                                PlotType (..), defaultStyle,
                                                terminal)
import qualified Graphics.Gnuplot.Simple       as GP
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Data.Concurrent.Adaptive.AdaptiveMap as AM
import qualified Data.Concurrent.Compact.AdaptiveMap  as CAM
import qualified Data.Concurrent.Ctrie                as CM
import qualified Data.Concurrent.PureMap              as PM
import qualified Data.Concurrent.PureMapL             as PML

{-# INLINABLE forkNIns #-}
forkNIns :: GenericImpl m -> Int -> Bench Measured
forkNIns GenericImpl { newMap, insert } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  let quota = fromIntegral $ ops `quot` splits
  measure' $ forkJoin splits $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
    for_ offset (offset + quota) $ \i -> insert i (i + 1) m

{-# INLINABLE forkNInsDel #-}
forkNInsDel :: GenericImpl m -> Int -> Bench Measured
forkNInsDel GenericImpl { newMap, insert, delete } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  let quota = fromIntegral $ ops `quot` splits
  measure' $ forkJoin splits $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
    for_ offset (offset + quota) $ \i -> if even chunk
                                           then delete i m
                                           else insert i (i + 1) m

{-# INLINABLE fork5050 #-}
fork5050 :: GenericImpl m -> Int -> Bench Measured
fork5050 GenericImpl { newMap, insert, delete } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !vec <- reader randomInts
  let quota = fromIntegral $ ops `quot` splits
  measure' $ forkJoin splits $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
        shouldDel = (vec VU.! (fromIntegral offset `mod` VU.length vec)) == 0
    for_ offset (offset + quota) $ \i -> if shouldDel
                                           then delete i m
                                           else insert i (i + 1) m

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

-- Fill with random data:
{-# INLINE fill #-}
fill :: m -> (Int64 -> Int64 -> m -> IO ()) -> Bench ()
fill !m insert = do
  !ps <- reader randomPairs
  liftIO $! VU.forM_ ps (\(k, v) -> insert k v m)

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

pureImpl :: GenericImpl (PM.PureMap Int64 Int64)
pureImpl = GenericImpl PM.newMap PM.get PM.ins PM.del nop PM.size (\_ -> return "_")

ctrieImpl = GenericImpl CM.empty CM.lookup CM.insert CM.delete nop CM.size (\_ -> return "_")

adaptiveImpl = GenericImpl AM.newMap AM.get AM.ins AM.del AM.transition AM.size AM.getState
-- Quick hack below, start in B state
-- ----------------------------------
-- Interestingly this is STILL very different perf wise from pure on
-- the following command, which seems bogus.
--     stack bench adaptive-hashmap:bench-adaptive-hashmap-1 '--benchmark-arguments=--ops=10000000 --bench=hotcold --runs=3 --minthreads=1 --maxthreads=12 --ratio=5000 --variants=adaptive +RTS -N12 -A100M -H4G -qa -s -ls'
-- adaptiveImpl = GenericImpl AM.newBMap AM.get AM.ins AM.del AM.transition AM.size
-- ----------------------------------

cadaptiveImpl = GenericImpl CAM.newMap CAM.get CAM.ins CAM.del CAM.transition CAM.size CAM.getState

-- | This runs the hot-phase, transition, cold-phase benchmark.
{-# INLINE hotCold #-}
hotCold :: GenericImpl m -> Int -> Bench Measured
hotCold GenericImpl { newMap, get, insert, delete, transition } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  fill m insert

  -- transition d m -- TEMP!  Transition before measuring.  This makes it much faster on 1 thread.
  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
      len = fromIntegral $ VU.length vec
  measure' $ forkJoin splits $ \chunk ->
    do
      let offset1 = fromIntegral $ chunk * fromIntegral quota
          offset2 = offset1 + phase1 + 1
      for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                                then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                                else rand range >>= \k -> insert k i m
      transition m
      fold offset2 (offset2 + phase2) 0 (\b -> maybe b (+ b)) $ \i ->
        if precompute
          then get (vec VU.! fromIntegral (i `mod` len)) m
          else rand range >>= \k -> get k m

{-# INLINABLE hotPhase #-}
hotPhase :: GenericImpl m -> Int -> Bench Measured
hotPhase GenericImpl { newMap, get, insert, delete, transition } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  fill m insert

  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      len = fromIntegral $ VU.length vec
  measure' $ forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
    for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                              then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                              else rand range >>= \k -> insert k i m

{-# INLINE coldPhase #-}
coldPhase :: GenericImpl m -> Int -> Bench Measured
coldPhase GenericImpl { newMap, get, insert, delete, transition, state, size } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  fill m insert

  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
      len = fromIntegral $ VU.length vec
  liftIO $ putStrLn $ "(cold:perthread,ops1/ops2 " ++ show phase1 ++ " " ++ show phase2 ++ ")"
  -- Run the hot phase and then use a barrier/join before measuring the cold phase:
  -- ----------------------------
  liftIO $ forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                              then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                              else rand range >>= \k -> insert k i m
    -- t <- measureOnce $ transition m when (measTime t > 0.001) $
    --   do putStr$  "(trans "++ show (measTime t)++") "; hFlush stdout
    -- st0 <- state m case st0 of
    --   "AB" -> error $ "coldPhase: transition returned locally on this thread, but state is: "++st0
    --   "A"  -> error $ "coldPhase: transition returned locally on this thread, but state is: "++st0
    --   _ -> return ()
    return ()
  liftIO $ do
    putStrLn "coldPhase: mutable phase done, transitioning..."
    hFlush stdout
    -- Since we're not measuring it anyway, do this transition after the barrier, on the main thread:
    t <- measureOnce $ transition m
    -- when (measTime t > 0.001) $
    putStrLn $ "  (trans " ++ show (measTime t) ++ ") "
    hFlush stdout

    sz <- size m
    st1 <- state m
    putStr $ "(size " ++ show sz ++ ", stateAfterTrans " ++ st1 ++ ") "

  measure' $ forkJoin splits $ \chunk ->
    do
      let offset1 = fromIntegral $ chunk * fromIntegral quota
          offset2 = offset1 + phase1 + 1
      fold offset2 (offset2 + phase2) 0 (\b -> maybe b (+ b)) $ \i ->
        if precompute
          then get (vec VU.! fromIntegral (i `mod` len)) m
          else rand range >>= \k -> get k m

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop !i
      | i > end = return ()
      | otherwise = fn i >> loop (i + 1)

{-# INLINABLE fold #-}
fold :: (Num n, Ord n, Monad m) => n -> n -> b -> (b -> a -> b) -> (n -> m a) -> m b
fold start end _ _ _
  | start > end = error "start greater than end"
fold start end z fld fn = loop start
  where
    loop !i
      | i > end = return z
      | otherwise = do
          !x <- fn i
          !xs <- loop (i + 1)
          return $! fld xs x

for :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m [a]
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
fori :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m [(n, a)]
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
  deriving (Eq, Ord, Read, Show)

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
getTime = ((1.0e-9 *) . fromInteger . C.timeSpecAsNanoSecs) `fmap` C.getTime C.Monotonic

getCPUTime :: IO Double
getCPUTime = ((1.0e-9 *) . fromInteger . C.timeSpecAsNanoSecs) `fmap` C.getTime C.ProcessCPUTime

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
measure :: MonadIO m => Int64 -> IO a -> m Measured
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
measureOnce :: MonadIO m => IO a -> m Measured
measureOnce = measure 1

-- | measure `iters` times and rescale
{-# INLINE measure' #-}
measure' :: IO a -> Bench Measured
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

{-# INLINE nop #-}
nop :: Applicative m => a -> m ()
nop _ = pure ()

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

{-# INLINE runAll #-}
runAll :: (Int -> Bench Measured) -> Bench [(Int, Measured)]
runAll !fn = do
  !minthreads <- reader minthreads
  !maxthreads <- reader maxthreads
  !runs <- reader runs
  !flag <- ask
  liftIO $! fori minthreads maxthreads $! \i -> do
    putStrLn $ "  Running threads = " ++ show i
    hFlush stdout
    t <- run runs $! runReaderT (fn i) flag
    putStrLn $ "\n  Time reported: " ++ show (measTime t) ++
                                        ", cycles: " ++ show (measCycles t)
    hFlush stdout
    return t

{-# INLINE benchmark #-}
benchmark :: String -> Bench [(Int, Measured)]
benchmark "pure" =
  reader bench >>= \bench -> case bench of
    "ins"     -> runAll $ forkNIns pureImpl
    "insdel"  -> runAll $ forkNInsDel pureImpl
    "random"  -> runAll $ fork5050 pureImpl
    "hotcold" -> runAll $ hotCold pureImpl
    "hot"     -> runAll $ hotPhase pureImpl
    "cold"    -> runAll $ coldPhase pureImpl
benchmark "ctrie" =
  reader bench >>= \bench -> case bench of
    "ins"     -> runAll $ forkNIns ctrieImpl
    "insdel"  -> runAll $ forkNInsDel ctrieImpl
    "random"  -> runAll $ fork5050 ctrieImpl
    "hotcold" -> runAll $ hotCold ctrieImpl
    "hot"     -> runAll $ hotPhase ctrieImpl
    "cold"    -> runAll $ coldPhase ctrieImpl
benchmark "adaptive" =
  reader bench >>= \bench -> case bench of
    "ins"     -> runAll $ forkNIns adaptiveImpl
    "insdel"  -> runAll $ forkNInsDel adaptiveImpl
    "random"  -> runAll $ fork5050 adaptiveImpl
    "hotcold" -> runAll $ hotCold adaptiveImpl
    "hot"     -> runAll $ hotPhase adaptiveImpl
    "cold"    -> runAll $ coldPhase adaptiveImpl
benchmark "c-adaptive" =
  reader bench >>= \bench -> case bench of
    "ins"     -> runAll $ forkNIns cadaptiveImpl
    "insdel"  -> runAll $ forkNInsDel cadaptiveImpl
    "random"  -> runAll $ fork5050 cadaptiveImpl
    "hotcold" -> runAll $ hotCold cadaptiveImpl
    "hot"     -> runAll $ hotPhase cadaptiveImpl
    "cold"    -> runAll $ coldPhase cadaptiveImpl
benchmark x =
  reader bench >>= \y -> error $ "benchmark: unknown arguments: " ++ show x ++ " " ++ show y

{-# INLINE rand #-}
rand :: Int64 -> IO Int64
rand !n = PCG.withSystemRandom $ PCG.uniformR (0, n)

main :: IO ()
main = do
  args <- CA.cmdArgs flag
  caps <- getNumCapabilities

  !gen <- PCG.createSystemRandom
  !randomInts <- VU.replicateM 100000 (PCG.uniformR (0, range args) gen :: IO Int64)
  !randomPairs <- VU.replicateM (initial args)
                    (PCG.uniformR ((0, 0), (range args, range args)) gen :: IO (Int64, Int64))

  let !opts = args
        { maxthreads = if maxthreads args <= 0
                         then caps
                         else maxthreads args
        , randomInts = randomInts
        , randomPairs = randomPairs
        }

  putStrLn $ "Operations:    " ++ show (ops opts)
  putStrLn $ "Number of runs:" ++ show (runs opts)
  putStrLn $ "File:          " ++ show (file opts)
  putStrLn $ "Output:        " ++ show (output opts)
  putStrLn $ "Benchmark:     " ++ show (bench opts)
  putStrLn $ "Variants:      " ++ show (variants opts)
  putStrLn $ "Ratio:         " ++ show (ratio opts)
  putStrLn $ "MinThreads:    " ++ show (minthreads opts)
  putStrLn $ "MaxThreads:    " ++ show (maxthreads opts)
  putStrLn $ "Key range:     " ++ show (range opts)
  putStrLn $ "Precompute:    " ++ show (precompute opts)
  hFlush stdout

  let vs = if allvariants opts
             then all_variants
             else variants opts
  !zs <- forM vs $! \variant -> do
           putStrLn $ "\n Running variant: " ++ variant
           hFlush stdout
           runReaderT (benchmark variant) opts

  let term = if output opts == "svg"
               then terminal (SVG.cons $ file opts ++ ".svg")
               else terminal (X11.persist X11.cons)

  when (doplot opts) $
    GP.plotPathsStyle
      [ XLabel "Threads"
      , YLabel "Time in seconds"
      , XTicks $ Just ["1"]
      , Title (bench opts)
      , Custom "grid" []
      , term
      , Custom "style line" ["3", "lc", "3", "lw", "3"]
      ]
      (map (style *** fmap (fromIntegral *** measTime)) (vs `zip` zs))

  where
    style v = defaultStyle { GP.plotType = LinesPoints, GP.lineSpec = CustomStyle [LineTitle v] }

data Flag =
       Flag
         { ops         :: !Int
         , file        :: !String
         , output      :: !String
         , runs        :: !Int
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
         , randomInts  :: VU.Vector Int64
         , randomPairs :: VU.Vector (Int64, Int64)
         }
  deriving (Eq, Show, CA.Data, CA.Typeable)

type Bench a = ReaderT Flag IO a

all_variants = ["pure", "ctrie", "adaptive", "c-adaptive"]

flag :: Flag
flag = Flag
  { ops = 100000 &= help "Total number of operations"
  , file = "report" &= help "Report file prefix"
  , output = "svg" &= help "Output {svg, x11}"
  , iters = 1 &= help "Number of iterations"
  , runs = 50 &= help "Number of runs"
  , bench = "hotcold" &= help "Benchmark {ins, insdel, random, hotcold, hot, cold}"
  , variants = [] &= help "Variants {nop, pure, cpure, ctrie, adaptive, c-adaptive}"
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
  }
