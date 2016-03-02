{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import qualified Control.Exception           as E
import           Control.Monad
import           Data.Int
import qualified Data.Vector.Storable        as VS
import           GHC.Stats                   (GCStats (..))
import qualified GHC.Stats                   as Stats
import           GHC.Word
import qualified System.Clock                as C
import           System.Console.CmdArgs
import           System.CPUTime.Rdtsc
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

import           Graphics.Gnuplot.Simple
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Control.Concurrent.Adaptive.AdaptiveMap as AM
import qualified Control.Concurrent.Compact.PureMap      as CPM
import qualified Control.Concurrent.Map                  as CM
import qualified Control.Concurrent.PureMap              as PM
import qualified Control.Concurrent.PureMapL             as PML

{-# INLINABLE insDelN #-}
insDelN :: IO m -> (Int64 -> Int64 -> m -> IO ()) -> (Int64 -> m -> IO ()) -> Int64 -> IO ()
insDelN newMap insert delete total = do
  m <- newMap
  for_ 1 total $ \i -> if even i
                         then delete i m
                         else insert i (i + 1) m

{-# INLINABLE forkNIns #-}
forkNIns :: IO m -> (Int64 -> Int64 -> m -> IO ()) -> Int -> Int -> IO ()
forkNIns newMap insert elems splits = do
  m <- newMap
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits
    (\chunk -> do
       let offset = fromIntegral $ chunk * fromIntegral quota
       for_ offset (offset + quota) $ \i -> insert i (i + 1) m)

{-# INLINABLE forkNInsDel #-}
forkNInsDel :: IO m -> (Int64 -> Int64 -> m -> IO ()) -> (Int64 -> m -> IO ()) -> Int -> Int -> IO ()
forkNInsDel newMap insert delete elems splits = do
  m <- newMap
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits
    (\chunk -> do
       let offset = fromIntegral $ chunk * fromIntegral quota
       for_ offset (offset + quota) $ \i -> if even chunk
                                              then delete i m
                                              else insert i (i + 1) m)

{-# INLINABLE fork5050 #-}
fork5050 :: IO m -> (Int64 -> Int64 -> m -> IO ()) -> (Int64 -> m -> IO ()) -> Int -> VS.Vector Int -> Int -> IO ()
fork5050 newMap insert delete elems vec splits = do
  m <- newMap
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits
    (\chunk -> do
       let offset = fromIntegral $ chunk * fromIntegral quota
           shouldDel = (vec VS.! (fromIntegral offset `mod` VS.length vec)) == 0
       for_ offset (offset + quota) $ \i -> if shouldDel
                                              then delete i m
                                              else insert i (i + 1) m)

{-# INLINABLE forkJoin #-}
forkJoin :: Int -> (Int -> IO ()) -> IO ()
forkJoin num act = loop num []
  where
    loop 0 ls = mapM_ takeMVar ls
    loop n ls = do
      mv <- newEmptyMVar
      _ <- forkOn (n - 1) $ do
             act (n - 1)
             putMVar mv ()
      loop (n - 1) (mv : ls)

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop !i
      | i > end = return ()
      | otherwise = fn i >> loop (i + 1)

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
          return $ (i, x) : xs

-- from criterion
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
  deriving (Eq, Read, Show)

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

{-# INLINE measure #-}
measure :: Int64 -> IO a -> IO Measured
measure !iters !f = do
  startStats <- getGCStats
  startTime <- getTime
  startCpuTime <- getCPUTime
  startCycles <- getCycles
  _ <- for_ 1 iters $ \_ -> f
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

randomInts :: IO (VS.Vector Int)
randomInts = do
  gen <- PCG.createSystemRandom
  VS.replicateM (100000 :: Int) (PCG.uniformR (0, 1) gen :: IO Int)

{-# INLINE benchmark #-}
benchmark :: String -> Flag -> IO [(Int, Measured)]
benchmark "pure" Flag { bench, runs, size, threads }
  | bench == "ins" = fori 1 threads $ measure runs . forkNIns PM.newMap PM.ins size
  | bench == "insdel" = fori 1 threads $ measure runs . forkNInsDel PM.newMap PM.ins PM.del size
  | bench == "random" = fori 1 threads . (measure runs .) . fork5050 PM.newMap PM.ins PM.del size =<< randomInts
benchmark "ctrie" Flag { bench, runs, size, threads }
  | bench == "ins" = fori 1 threads $ measure runs . forkNIns CM.empty CM.insert size
  | bench == "insdel" = fori 1 threads $ measure runs . forkNInsDel CM.empty CM.insert CM.delete
                                                          size
  | bench == "random" = fori 1 threads .
                        (measure runs .) .
                        fork5050 CM.empty CM.insert CM.delete size =<< randomInts
benchmark "adaptive" Flag { bench, runs, size, threads }
  | bench == "ins" = fori 1 threads $ measure runs . forkNIns AM.newMap AM.ins size
  | bench == "insdel" = fori 1 threads $ measure runs . forkNInsDel AM.newMap AM.ins AM.del size
  | bench == "random" = fori 1 threads . (measure runs .) . fork5050 AM.newMap AM.ins AM.del size =<< randomInts
benchmark _ _ = error "unknown"

main :: IO ()
main = do
  args <- cmdArgs flag
  caps <- getNumCapabilities
  let !opts = if (threads args) <= 0
                then args { threads = caps }
                else args

  putStrLn $ "Key size:      " ++ show (size opts)
  putStrLn $ "Number of runs:" ++ show (runs opts)
  putStrLn $ "Warmup runs:   " ++ show (warmup opts)
  putStrLn $ "File:          " ++ show (file opts)
  putStrLn $ "Output:        " ++ show (output opts)
  putStrLn $ "Benchmark:     " ++ show (bench opts)
  putStrLn $ "Variants:      " ++ show (variants opts)
  putStrLn $ "Threads:       " ++ show (threads opts)
  hFlush stdout

  let vs = variants opts
  !zs <- forM vs $ \variant -> benchmark variant opts

  let term = if (output opts) == "svg"
               then terminal $ (SVG.cons $ file opts ++ ".svg")
               else terminal $ (X11.persist $ X11.cons)

  plotListsStyle
    [ XLabel "Threads"
    , YLabel "Time in seconds"
    , XTicks $ Just ["1"]
    , Title (bench opts)
    , Custom "grid" []
    , term
    , Custom "style line" ["3", "lc", "3", "lw", "3"]
    ]
    (map (\(v, z) -> (style v, (measTime . snd) `fmap` z)) (vs `zip` zs))

  where
    style v = defaultStyle { plotType = LinesPoints, lineSpec = CustomStyle [LineTitle v] }

data Flag =
       Flag
         { size     :: Int
         , file     :: String
         , output   :: String
         , runs     :: Int64
         , warmup   :: Int
         , bench    :: String
         , variants :: [String]
         , threads  :: Int
         }
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag
  { warmup = 5 &= help "Number of warmup runs"
  , size = 100000 &= help "Key size"
  , file = "report" &= help "Report file prefix"
  , output = "svg" &= help "Output {svg, x11}"
  , runs = 50 &= help "Number of runs"
  , bench = "insdel" &= help "Benchmark {ins, insdel, random}"
  , variants = ["pure", "ctrie", "adaptive"] &= help "Variants {nop, pure, cpure, ctrie, adaptive}"
  , threads = 0 &= help "Number of threads"
  }
