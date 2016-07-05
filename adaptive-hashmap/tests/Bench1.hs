{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE Strict               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | This is a second (modified) version of the benchmark harness by Vikraman
--   It measures time to perform a certain number of read/write operations.

module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Reader
import           Data.Int
import qualified Data.Vector.Unboxed         as VU
import qualified System.Console.CmdArgs      as CA
import           System.Directory
import           System.IO
import qualified System.Random.PCG.Fast.Pure as PCG

import           Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.File                 as File
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.Option         as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG

import GHC.Stats (getGCStats)

import Types

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

-- Fill with random data:
{-# INLINE fill #-}
fill :: m -> (Int64 -> Int64 -> m -> IO ()) -> Bench ()
fill !m insert = do
  !ps <- reader randomPairs
  liftIO $! VU.forM_ ps (\(k, v) -> insert k v m)

-- | This runs the hot-phase, transition, cold-phase benchmark.
{-# INLINE hotCold #-}
hotCold :: GenericImpl m -> Int -> Bench Measured
hotCold GenericImpl { newMap, get, insert, transition } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  !seed <- reader seed
  fill m insert

  -- transition d m -- TEMP!  Transition before measuring.  This makes it much faster on 1 thread.
  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
      len = fromIntegral $ VU.length vec
  measure' $ forkJoin splits $ \chunk ->
    do
      !g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
      let offset1 = fromIntegral $ chunk * fromIntegral quota
          offset2 = offset1 + phase1 + 1
      for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                                then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                                else rand g range >>= \k -> insert k i m
      transition m
      fold offset2 (offset2 + phase2) 0 (\b -> maybe b (+ b)) $ \i ->
        if precompute
          then get (vec VU.! fromIntegral (i `mod` len)) m
          else rand g range >>= \k -> get k m

{-# INLINABLE hotPhase #-}
hotPhase :: GenericImpl m -> Int -> Bench Measured
hotPhase GenericImpl { newMap, get, insert, transition } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  !seed <- reader seed
  fill m insert

  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      len = fromIntegral $ VU.length vec
  measure' $ forkJoin splits $ \chunk -> do
    !g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    let offset1 = fromIntegral $ chunk * fromIntegral quota
    for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                              then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                              else rand g range >>= \k -> insert k i m

{-# INLINE coldPhase #-}
coldPhase :: GenericImpl m -> Int -> Bench Measured
coldPhase GenericImpl { newMap, get, insert, transition, state, size } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  !seed <- reader seed
  fill m insert

  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
      len = fromIntegral $ VU.length vec
  liftIO $ putStrLn $ "(cold:perthread,ops1/ops2 " ++ show phase1 ++ " " ++ show phase2 ++ ")"
  -- Run the hot phase and then use a barrier/join before measuring the cold phase:
  -- ----------------------------
  liftIO $ forkJoin splits $ \chunk -> do
    !g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                              then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                              else rand g range >>= \k -> insert k i m
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
      !g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
      let offset1 = fromIntegral $ chunk * fromIntegral quota
          offset2 = offset1 + phase1 + 1

      if precompute -- Lend the compiler a hand; lift this branch out of the loop.
      then for_ offset2 (offset2 + phase2) $ \i ->
            do let ix = (vec VU.! fromIntegral (i `mod` len))
               get ix m

      else for_ offset2 (offset2 + phase2) $ \_ ->
            do ix <- rand g range
               get ix m

-- [2016.06.25] This is non-tail-recursive:
      -- fold offset2 (offset2 + phase2) 0 (\b -> maybe b (+ b)) $ \i ->
      --   if precompute
      --     then get (vec VU.! fromIntegral (i `mod` len)) m
      --     else rand g range >>= \k -> get k m

{-# INLINE transitionPhase #-}
transitionPhase :: GenericImpl m -> Int -> Bench Measured
transitionPhase GenericImpl { newMap, get, insert, transition } splits = do
  !m <- liftIO newMap
  !ops <- reader ops
  !ratio <- reader ratio
  !vec <- reader randomInts
  !range <- reader range
  !precompute <- reader precompute
  !seed <- reader seed
  fill m insert

  let quota = fromIntegral $ ops `quot` splits
      phase1 = quota `quot` ratio
      phase2 = (quota * (ratio - 1)) `quot` ratio
      len = fromIntegral $ VU.length vec
  liftIO $ forkJoin splits $ \chunk -> do
    !g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    let offset1 = fromIntegral $ chunk * fromIntegral quota
        offset2 = offset1 + phase1 + 1
    for_ offset1 (offset1 + phase1) $ \i -> if precompute
                                              then insert (vec VU.! fromIntegral (i `mod` len)) i m
                                              else rand g range >>= \k -> insert k i m
  measure' $ forkJoin splits $ \chunk -> transition m

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
    putStrLn $ "\n  Time reported: " ++ show (measTime t)
      ++ ", cycles: " ++ show (measCycles t) ++ ", numGcs: " ++ show (measNumGcs t)
      ++ ", allocated: " ++ show (measAllocated t) ++ ", gcCpuSeconds: " ++ show (measGcCpuSeconds t)
      ++ ", copied: " ++ show (measBytesCopied t)
    hFlush stdout
    return t

{-# INLINE benchmark #-}
benchmark :: String -> Bench [(Int, Measured)]
benchmark "nop" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns nopImpl
    "insdel"     -> runAll $ forkNInsDel nopImpl
    "random"     -> runAll $ fork5050 nopImpl
    "hotcold"    -> runAll $ hotCold nopImpl
    "hot"        -> runAll $ hotPhase nopImpl
    "cold"       -> runAll $ coldPhase nopImpl
    "transition" -> runAll $ transitionPhase nopImpl
benchmark "pure" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns pureImpl
    "insdel"     -> runAll $ forkNInsDel pureImpl
    "random"     -> runAll $ fork5050 pureImpl
    "hotcold"    -> runAll $ hotCold pureImpl
    "hot"        -> runAll $ hotPhase pureImpl
    "cold"       -> runAll $ coldPhase pureImpl
    "transition" -> runAll $ transitionPhase pureImpl
benchmark "ctrie" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns ctrieImpl
    "insdel"     -> runAll $ forkNInsDel ctrieImpl
    "random"     -> runAll $ fork5050 ctrieImpl
    "hotcold"    -> runAll $ hotCold ctrieImpl
    "hot"        -> runAll $ hotPhase ctrieImpl
    "cold"       -> runAll $ coldPhase ctrieImpl
    "transition" -> runAll $ transitionPhase ctrieImpl
benchmark "adaptive" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns adaptiveImpl
    "insdel"     -> runAll $ forkNInsDel adaptiveImpl
    "random"     -> runAll $ fork5050 adaptiveImpl
    "hotcold"    -> runAll $ hotCold adaptiveImpl
    "hot"        -> runAll $ hotPhase adaptiveImpl
    "cold"       -> runAll $ coldPhase adaptiveImpl
    "transition" -> runAll $ transitionPhase adaptiveImpl
benchmark "cc-adaptive" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns ccadaptiveImpl
    "insdel"     -> runAll $ forkNInsDel ccadaptiveImpl
    "random"     -> runAll $ fork5050 ccadaptiveImpl
    "hotcold"    -> runAll $ hotCold ccadaptiveImpl
    "hot"        -> runAll $ hotPhase ccadaptiveImpl
    "cold"       -> runAll $ coldPhase ccadaptiveImpl
    "transition" -> runAll $ transitionPhase ccadaptiveImpl
benchmark "pc-adaptive" =
  reader bench >>= \bench -> case bench of
    "ins"        -> runAll $ forkNIns pcadaptiveImpl
    "insdel"     -> runAll $ forkNInsDel pcadaptiveImpl
    "random"     -> runAll $ fork5050 pcadaptiveImpl
    "hotcold"    -> runAll $ hotCold pcadaptiveImpl
    "hot"        -> runAll $ hotPhase pcadaptiveImpl
    "cold"       -> runAll $ coldPhase pcadaptiveImpl
    "transition" -> runAll $ transitionPhase pcadaptiveImpl
benchmark x =
  reader bench >>= \y -> error $ "benchmark: unknown arguments: " ++ show x ++ " " ++ show y

main :: IO ()
main = do
  args <- CA.cmdArgs flag
  caps <- getNumCapabilities

  putStrLn $ "Creating random Ints of size: "++show (maxsize args)

  !gen <- PCG.createSystemRandom
  -- !randomInts <- VU.replicateM (maxsize args) (PCG.uniformR (0, range args) gen :: IO Int64)
  -- !randomPairs <- VU.replicateM (initial args)
  --                   (PCG.uniformR ((0, 0), (range args, range args)) gen :: IO (Int64, Int64))
  let howMuchRandomness = 10000

  !randomInts <- VU.replicateM howMuchRandomness (PCG.uniformR (0, range args) gen :: IO Int64)
  !randomPairs <- VU.replicateM howMuchRandomness
                    (PCG.uniformR ((0, 0), (range args, range args)) gen :: IO (Int64, Int64))

  -- let randomInts = error "don't use randomInts"
  --     randomPairs = error "don't use randomPairs"

  let !opts = args
        { maxthreads = if maxthreads args <= 0
                         then caps
                         else maxthreads args
        , randomInts = randomInts
        , randomPairs = randomPairs
        }

  let vs = if allvariants opts
             then all_variants
             else variants opts

  putStrLn $ "Operations:    " ++ show (ops opts)
  putStrLn $ "Number of runs:" ++ show (runs opts)
  putStrLn $ "File:          " ++ show (file opts)
  putStrLn $ "Benchmark:     " ++ show (bench opts)
  putStrLn $ "Variants:      " ++ show vs
  putStrLn $ "Ratio:         " ++ show (ratio opts)
  putStrLn $ "MinThreads:    " ++ show (minthreads opts)
  putStrLn $ "MaxThreads:    " ++ show (maxthreads opts)
  putStrLn $ "Key range:     " ++ show (range opts)
  putStrLn $ "Precompute:    " ++ show (precompute opts)
  hFlush stdout

  !zs <- forM vs $! \variant -> do
           putStrLn $ "\n Running variant: " ++ variant
           hFlush stdout
           runReaderT (benchmark variant) opts

  let points = map (map (\(t, m) -> (t, measTime m))) zs
      term = SVG.cons $ file opts ++ ".svg"
      frameOpts = Opts.xLabel "Threads" $ Opts.yLabel "Time in seconds" $
        Opts.title (bench opts) $ Opts.grid True $ Opts.add (Opt.xTicks "") ["1"] $
          Opts.yFormat "%g" $ Opts.deflt
      lineSpecs = (\s -> LineSpec.lineWidth 1.5 $ LineSpec.pointSize 1 $
                     LineSpec.pointType s LineSpec.deflt) `map` ([4, 7, 8, 5, 1, 1] ++ [1,1 ..])
      frame = Frame.cons frameOpts $
        foldMap
          (\(v, ps, ls) -> (Graph2D.lineSpec $ LineSpec.title v ls)
                           `fmap`
                           Plot2D.list Graph2D.errorLines ps) $
          zip3 vs points lineSpecs


  when (export opts) $ do
    let (script, files) = fileContents (file opts) term frame
    createDirectoryIfMissing True (file opts)
    writeFile (file opts ++ ".plt") script
    mapM_ File.write files

  when (doplot opts) $
    void $ plot term frame
