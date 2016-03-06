{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This is a second (modified) version of the benchmark harness by Vikraman

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Extra
import qualified Control.Exception           as E
import           Control.Monad
import           Data.Int
import           Data.List hiding (insert)
import           Data.Monoid
import qualified Data.Vector.Unboxed         as VU
import           GHC.Stats                   (GCStats (..))
import qualified GHC.Stats                   as Stats
import           GHC.Word
import qualified System.Clock                as C
import qualified System.Console.CmdArgs      as CA
import           System.Console.CmdArgs      ((&=), help)
import           System.CPUTime.Rdtsc
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified Graphics.Gnuplot.Simple       as GP
import           Graphics.Gnuplot.Simple
                 (defaultStyle, terminal,
                  LineAttr(LineTitle), LineSpec(CustomStyle),
                          Attribute(..), PlotType(..))
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.X11 as X11

import qualified Control.Concurrent.Adaptive.AdaptiveMap as AM
import qualified Control.Concurrent.Compact.AdaptiveMap  as CAM
-- RRN: Uh, why this redundant copy?
-- import qualified Control.Concurrent.Map                  as CM
import qualified Control.Concurrent.Adaptive.Ctrie       as CM
import qualified Control.Concurrent.PureMap              as PM
import qualified Control.Concurrent.PureMapL             as PML

{-# INLINABLE forkNIns #-}
forkNIns :: IO m
         -> (Int64 -> Int64 -> m -> IO ())
         -> Int -> Int -> IO Measured
forkNIns newMap insert ops splits = do
  !m <- newMap
  let quota = fromIntegral $ ops `quot` splits
  measureOnce $ forkJoin splits $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
    for_ offset (offset + quota) $ \i -> insert i (i + 1) m

{-# INLINABLE forkNInsDel #-}
forkNInsDel :: IO m
            -> (Int64 -> Int64 -> m -> IO ())
            -> (Int64 -> m -> IO ())
            -> Int -> Int -> IO Measured
forkNInsDel newMap insert delete ops splits = do
  !m <- newMap
  let quota = fromIntegral $ ops `quot` splits
  measureOnce $ forkJoin splits $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
    for_ offset (offset + quota) $ \i -> if even chunk
                                           then delete i m
                                           else insert i (i + 1) m

{-# INLINABLE fork5050 #-}
fork5050 :: IO m
         -> (Int64 -> Int64 -> m -> IO ())
         -> (Int64 -> m -> IO ())
         -> Int -> IO (VU.Vector Int) -> Int -> IO Measured
fork5050 newMap insert delete ops newVec splits = do
  !m <- newMap
  !vec <- newVec
  let quota = fromIntegral $ ops `quot` splits
  measureOnce $ forkJoin splits $ \chunk -> do
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

{-# INLINE fill #-}
fill :: m -> (Int64 -> Int64 -> m -> IO ()) -> IO ()
fill !m insert = do
  !ps <- randomPairs
  VU.forM_ ps $! \(k, v) -> insert k v m


data GenericImpl m =
  GenericImpl
  { newMap :: IO m
  , get    :: Int64 -> m -> IO (Maybe Int64)
  , insert :: (Int64 -> Int64 -> m -> IO ())
  , delete :: (Int64 -> m -> IO ())
  , transition :: (m -> IO ())
  , size   :: m -> IO Int
  }

pureImpl :: GenericImpl (PM.PureMap Int64 Int64)
pureImpl = GenericImpl PM.newMap PM.get PM.ins PM.del nop PM.size

ctrieImpl = GenericImpl CM.empty CM.lookup CM.insert CM.delete nop CM.size

adaptiveImpl = GenericImpl AM.newMap AM.get AM.ins AM.del AM.transition AM.size

cadaptiveImpl = GenericImpl CAM.newMap CAM.get CAM.ins CAM.del CAM.transition CAM.size

-- | This runs the hot-phase, transition, cold-phase benchmark.
{-# INLINABLE hotCold #-}
hotCold :: IO m
        -> (Int64 -> m -> IO (Maybe Int64))
        -> (Int64 -> Int64 -> m -> IO ())
        -> (Int64 -> m -> IO ())
        -> (m -> IO ())
        -> Int -> Int64 -> Int -> IO Measured
hotCold newMap get insert delete transition ops ratio splits = do
  !m <- newMap
  fill m insert
  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
  measureOnce $ forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset1 (offset1 + phase1) $ \i -> insert i (i + 1) m
    transition m
    for_ offset2 (offset2 + phase2) $ \i -> get i m

{-# INLINABLE hotPhase #-}
hotPhase :: IO m
         -> (Int64 -> m -> IO (Maybe Int64))
         -> (Int64 -> Int64 -> m -> IO ())
         -> (Int64 -> m -> IO ())
         -> (m -> IO ())
         -> Int -> Int64 -> Int -> IO Measured
hotPhase newMap get insert delete transition ops ratio splits = do
  !m <- newMap
  fill m insert
  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
  measureOnce $ forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
    for_ offset1 (offset1 + phase1) $ \i -> insert i (i + 1) m

{-# INLINE coldPhase #-}
coldPhase :: GenericImpl m 
          -> Int -> Int64 -> Int -> IO Measured
coldPhase d ops ratio splits = do
  !m <- newMap d
  fill m (insert d)
  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
  -- Run the hot phase and then use a barrier/join before measuring the cold phase:
  forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset1 (offset1 + phase1) $ \i -> insert d i (i + 1) m
    t <- measureOnce $ transition d m
    when (measTime t > 0.001) $ 
      do putStr$  "(trans "++ show (measTime t)++") "; hFlush stdout
    return ()
  sz <- size d m
  putStr$  "(size "++show sz++") "
  measureOnce $ forkJoin splits $ \chunk -> do
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset2 (offset2 + phase2) $ \i -> get d i m

{-# INLINABLE for_ #-}
for_ :: (Num n, Ord n, Monad m) => n -> n -> (n -> m a) -> m ()
for_ start end _
  | start > end = error "start greater than end"
for_ start end fn = loop start
  where
    loop !i
      | i > end = return ()
      | otherwise = fn i >> loop (i + 1)

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
          return $ x : xs

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

{-# INLINE measure #-}
measure :: Int64 -> IO a -> IO Measured
measure !iters !f = do
  performGC
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

{-# INLINE measureOnce #-}
measureOnce :: IO a -> IO Measured
measureOnce = measure 1

randomInts :: IO (VU.Vector Int)
randomInts = do
  gen <- PCG.createSystemRandom
  VU.replicateM 100000 (PCG.uniformR (0, 1) gen :: IO Int)

randomPairs :: IO (VU.Vector (Int64, Int64))
randomPairs = do
  gen <- PCG.createSystemRandom
  VU.replicateM 10000 (PCG.uniform gen :: IO (Int64, Int64))

{-# INLINE nop #-}
nop :: Applicative m => a -> m ()
nop _ = pure ()

-- | Run a function several times and take the median time.
{-# INLINE run #-}
run :: Int -> IO Measured -> IO Measured
run runs fn = do
  let rs = if even runs
             then (runs + 1)
             else runs
      mid = 1 + rs `quot` 2
  ms <- for 1 rs $ \_ -> fn
  return $! sort ms !! mid

{-# INLINE runAll #-}
runAll :: Int -> Int -> (Int -> IO Measured) -> IO [(Int, Measured)]
runAll threads runs fn =
   fori 1 threads $! \i' ->
     do let i = threads - i' + 1 -- Go backwards.  When running by hand I want to see the max threads first.
        putStrLn $ "  Running threads = " ++ show i; hFlush stdout
        t <- run runs $ fn i
        putStrLn $ "\n  Time reported: "++ show (measTime t) ++
                         ", cycles: " ++ show (measCycles t)
        hFlush stdout
        return t        

{-# INLINE benchmark #-}
benchmark :: String -> Flag -> IO [(Int, Measured)]
benchmark "pure" Flag { bench, runs, ops, threads, ratio }
  | bench == "ins" = runAll threads runs $ forkNIns PM.newMap PM.ins ops
  | bench == "insdel" = runAll threads runs $ forkNInsDel PM.newMap PM.ins PM.del ops
  | bench == "random" = runAll threads runs $ fork5050 PM.newMap PM.ins PM.del ops randomInts
  | bench == "hotcold" = runAll threads runs $ hotCold PM.newMap PM.get PM.ins PM.del nop ops ratio
  | bench == "hot" = runAll threads runs $ hotPhase PM.newMap PM.get PM.ins PM.del nop ops ratio
  | bench == "cold" = runAll threads runs $ coldPhase pureImpl ops ratio
benchmark "ctrie" Flag { bench, runs, ops, threads, ratio }
  | bench == "ins" = runAll threads runs $ forkNIns CM.empty CM.insert ops
  | bench == "insdel" = runAll threads runs $ forkNInsDel CM.empty CM.insert CM.delete ops
  | bench == "random" = runAll threads runs $ fork5050 CM.empty CM.insert CM.delete ops randomInts
  | bench == "hotcold" = runAll threads runs $ hotCold CM.empty CM.lookup CM.insert CM.delete nop ops ratio
  | bench == "hot" = runAll threads runs $ hotPhase CM.empty CM.lookup CM.insert CM.delete nop ops ratio
  | bench == "cold" = runAll threads runs $ coldPhase ctrieImpl ops ratio
benchmark "adaptive" Flag { bench, runs, ops, threads, ratio }
  | bench == "ins" = runAll threads runs $ forkNIns AM.newMap AM.ins ops
  | bench == "insdel" = runAll threads runs $ forkNInsDel AM.newMap AM.ins AM.del ops
  | bench == "random" = runAll threads runs $ fork5050 AM.newMap AM.ins AM.del ops randomInts
  | bench == "hotcold" = runAll threads runs $ hotCold AM.newMap AM.get AM.ins AM.del nop ops ratio
  | bench == "hot" = runAll threads runs $ hotPhase AM.newMap AM.get AM.ins AM.del nop ops ratio
  | bench == "cold" = runAll threads runs $ coldPhase adaptiveImpl ops ratio
benchmark "c-adaptive" Flag { bench, runs, ops, threads, ratio }
  | bench == "ins" = runAll threads runs $ forkNIns CAM.newMap CAM.ins ops
  | bench == "insdel" = runAll threads runs $ forkNInsDel CAM.newMap CAM.ins CAM.del ops
  | bench == "random" = runAll threads runs $ fork5050 CAM.newMap CAM.ins CAM.del ops randomInts
  | bench == "hotcold" = runAll threads runs $ hotCold CAM.newMap CAM.get CAM.ins CAM.del nop ops ratio
  | bench == "hot" = runAll threads runs $ hotPhase CAM.newMap CAM.get CAM.ins CAM.del nop ops ratio
  | bench == "cold" = runAll threads runs $ coldPhase cadaptiveImpl ops ratio
benchmark x y = error$ "benchmark: unknown argumetns: "++show x++" "++show y

main :: IO ()
main = do
  args <- CA.cmdArgs flag
  caps <- getNumCapabilities
  let !opts = if (threads args) <= 0
                then args { threads = caps }
                else args

  putStrLn $ "Operations:    " ++ show (ops opts)
  putStrLn $ "Number of runs:" ++ show (runs opts)
  putStrLn $ "File:          " ++ show (file opts)
  putStrLn $ "Output:        " ++ show (output opts)
  putStrLn $ "Benchmark:     " ++ show (bench opts)
  putStrLn $ "Variants:      " ++ show (variants opts)
  putStrLn $ "Ratio:         " ++ show (ratio opts)
  putStrLn $ "Threads:       " ++ show (threads opts)
  hFlush stdout

  let vs = if allvariants opts
           then all_variants
           else variants opts
  !zs <- forM vs $! \variant -> do putStrLn $ "\n Running variant: "++ variant
                                   hFlush stdout
                                   benchmark variant opts

  let term = if (output opts) == "svg"
               then terminal $ (SVG.cons $ file opts ++ ".svg")
               else terminal $ (X11.persist $ X11.cons)

  GP.plotPathsStyle
    [ XLabel "Threads"
    , YLabel "Time in seconds"
    , XTicks $ Just ["1"]
    , Title (bench opts)
    , Custom "grid" []
    , term
    , Custom "style line" ["3", "lc", "3", "lw", "3"]
    ]
    (map (\(v, z) -> (style v, (\(t, m) -> (fromIntegral t, measTime m)) `fmap` z)) (vs `zip` zs))

  where
    style v = defaultStyle { GP.plotType = LinesPoints
                           , GP.lineSpec = CustomStyle [LineTitle v] }

data Flag =
       Flag
         { ops      :: Int
         , file     :: String
         , output   :: String
         , runs     :: Int
         , bench    :: String
         , variants :: [String]
         , allvariants :: Bool
         , ratio    :: Int64
         , threads  :: Int
         }
  deriving (Eq, Show, CA.Data, CA.Typeable)

all_variants = ["pure", "ctrie", "adaptive", "c-adaptive"]

flag :: Flag
flag = Flag
  { ops = 100000 &= help "Total number of operations"
  , file = "report" &= help "Report file prefix"
  , output = "svg" &= help "Output {svg, x11}"
  , runs = 50 &= help "Number of runs"
  , bench = "hotcold" &= help "Benchmark {ins, insdel, random, hotcold, hot, cold}"
  , variants = [] &= help "Variants {nop, pure, cpure, ctrie, adaptive, c-adaptive}"
  , allvariants = False &= help "Use all builtin variants"
  , ratio = 200 &= help "Cold-to-hot ops ratio"
  , threads = 0 &= help "Number of threads"
  }
