{-# LANGUAGE Strict #-}

-- | coldPhase only.  Fix the number of threads, perform exactly N
-- reads on an N-size data structure.  Vary the size N.

module Main where

import           Control.Monad
import           Data.Int
import           Data.Word
import           System.Directory
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure as PCG

import           Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.File                 as File
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG

import qualified Data.Concurrent.Compact.Adaptive.PureToCompact as PCM
import qualified Data.Concurrent.PureMap                        as PM

import           Types (Measured (..))
import qualified Types as T

{-# NOINLINE threads #-}
threads :: Int
threads = unsafePerformIO $ read <$> getEnv "THREADS"

seed :: Word64
seed = 4096

range :: Int64
range = 2 ^ 10

{-# NOINLINE size #-}
size :: Int64
size = unsafePerformIO $ read <$> getEnv "SIZE"

{-# NOINLINE incr #-}
incr :: Int64
incr = unsafePerformIO $ read <$> getEnv "INCR"

{-# NOINLINE iters #-}
iters :: Int64
iters = unsafePerformIO $ read <$> getEnv "ITERS"

{-# INLINE measure #-}
measure :: IO a -> IO Measured
measure f = do
  m <- T.measure iters f
  return $ T.rescale m

nopBench :: IO [(Int64, Measured)]
nopBench = T.fori' 0 size incr $
  \n -> measure . T.forkJoin threads $
    \chunk -> do
      g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
      T.for_ 0 n $ \v -> do
        _ <- T.rand g range
        return ()

pureBench :: IO [(Int64, Measured)]
pureBench = T.fori' 0 size incr $ \n -> do
  pm <- PM.newMap
  void . T.forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    T.for_ 0 n $ \v -> do
      k <- T.rand g range
      PM.ins k v pm
  measure . T.forkJoin threads $
    \chunk -> do
      g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
      T.for_ 0 n $ \v -> do
        ix <- T.rand g range
        PM.get ix pm

cnfpureBench :: IO [(Int64, Measured)]
cnfpureBench = T.fori' 0 size incr $ \n -> do
  pcm <- PCM.newMap
  void . T.forkJoin threads $ \chunk -> do
    g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
    T.for_ 0 n $ \v -> do
      k <- T.rand g range
      PCM.ins k v pcm
  PCM.transition pcm
  measure . T.forkJoin threads $
    \chunk -> do
      g <- PCG.restore $ PCG.initFrozen (seed + fromIntegral chunk)
      T.for_ 0 n $ \v -> do
        ix <- T.rand g range
        PCM.get ix pcm

benchmark :: String -> IO [(Int64, Measured)]
benchmark "nop" = nopBench
benchmark "pure" = pureBench
benchmark "cnfpure" = cnfpureBench
benchmark _ = error "unknown variant"

main :: IO ()
main = do
  let vs = ["nop", "pure", "cnfpure"]
  zs <- forM vs benchmark

  let points = map (map (\(s, m) -> (s, measBytesCopied m))) zs
      term = SVG.cons "report.svg"
      frameOpts = Opts.xLabel "Size" $ Opts.yLabel "GC time in seconds" $
        Opts.title "cold GC" $ Opts.grid True $ Opts.yFormat "%g" Opts.deflt
      lineSpecs = (\s -> LineSpec.lineWidth 1.5 $ LineSpec.pointSize 1 $
                     LineSpec.pointType s LineSpec.deflt) `map` ([4, 7, 8, 5, 1, 1] ++ [1,1 ..])
      frame = Frame.cons frameOpts $
        foldMap
          (\(v, ps, ls) -> (Graph2D.lineSpec $ LineSpec.title v ls)
                           `fmap`
                           Plot2D.list Graph2D.errorLines ps) $
          zip3 vs points lineSpecs

  let (script, files) = fileContents "report" term frame
  createDirectoryIfMissing True "report"
  writeFile "report.plt" script
  mapM_ File.write files

  void $ plot term frame
