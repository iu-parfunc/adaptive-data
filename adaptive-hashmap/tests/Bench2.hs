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

import Types

transitionPhase :: GenericImpl m -> Int -> Bench Measured
transitionPhase GenericImpl { newMap, insert, transition } size = do
  !m <- liftIO newMap
  !vec <- reader randomInts
  !threads <- reader maxthreads
  let quota = fromIntegral $ size `quot` threads
      len = fromIntegral $ VU.length vec
  liftIO $ forkJoin threads $ \chunk -> do
    let offset = fromIntegral $ chunk * fromIntegral quota
    for_ offset (offset + quota) $ \i -> insert (vec VU.! fromIntegral (i `mod` len)) i m
  measure' $ transition m

{-# INLINE runAll #-}
runAll :: (Int -> Bench Measured) -> Bench [(Int, Measured)]
runAll !fn = do
  !size <- reader ops
  !maxsize <- reader maxsize
  !stepsize <- reader stepsize
  !runs <- reader runs
  !flag <- ask
  liftIO $! fori' size maxsize stepsize $! \i -> do
    putStrLn $ "  Running size = " ++ show i
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
    "transition" -> runAll $ transitionPhase pureImpl
benchmark "ctrie" =
  reader bench >>= \bench -> case bench of
    "transition" -> runAll $ transitionPhase ctrieImpl
benchmark "adaptive" =
  reader bench >>= \bench -> case bench of
    "transition" -> runAll $ transitionPhase adaptiveImpl
benchmark "c-adaptive" =
  reader bench >>= \bench -> case bench of
    "transition" -> runAll $ transitionPhase cadaptiveImpl
benchmark x =
  reader bench >>= \y -> error $ "benchmark: unknown arguments: " ++ show x ++ " " ++ show y

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
  putStrLn $ "MinSize:       " ++ show (ops opts)
  putStrLn $ "MaxSize:       " ++ show (maxsize opts)
  putStrLn $ "StepSize:      " ++ show (stepsize opts)
  hFlush stdout

  !zs <- forM vs $! \variant -> do
           putStrLn $ "\n Running variant: " ++ variant
           hFlush stdout
           runReaderT (benchmark variant) opts

  let points = map (map (\(t, m) -> (t, measTime m))) zs
      term = SVG.cons $ file opts ++ ".svg"
      frameOpts = Opts.xLabel "Size" $ Opts.yLabel "Time in seconds" $
        Opts.title (bench opts) $ Opts.grid True $ Opts.yFormat "%g" Opts.deflt
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
