{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Extra
--import qualified Data.Atomics.Counter as C
import           Data.Int
import           Data.IORef
import           Data.Time.Clock
import           GHC.Word
import           System.Console.CmdArgs
import           System.IO
import           System.Mem
import qualified System.Random.PCG.Fast.Pure as PCG

import qualified Control.Concurrent.Adaptive.AdaptiveMap as AM
import qualified Control.Concurrent.Compact.PureMap      as CPM
import qualified Control.Concurrent.Map                  as CM
import qualified Control.Concurrent.PureMap              as PM
import qualified Control.Concurrent.PureMapL             as PML

thread :: Int -> [Int64 -> Int64 -> IO()] -> [Double] -> Flag -> Barrier Bool -> Word64 -> IO(Int)
thread !_ !ops !ratios !option !bar !seedn = do
  !g <- PCG.restore $ PCG.initFrozen seedn
  let loop !i = do
        if i > 0
          then do
          !p <- PCG.uniformRD (0.0, 1.0) g :: IO Double
          !k <- PCG.uniformRI64 (0, range option) g :: IO Int64
          !v <- PCG.uniformRI64 (0, range option) g :: IO Int64
          let op = ops !! snd (foldl (\(z,j) -> \x ->
                                       if (x+z>=p)
                                       then (x+z,j)
                                       else (x+z,j+1))
                               (0.0::Double,0) ratios)
          op k v
          loop $ i - 1
          else return ()

  let read_loop !i = do
        if i > 0
          then do
          !p <- PCG.uniformRD (0.0, 1.0) g :: IO Double
          !k <- PCG.uniformRI64 (0, range option) g :: IO Int64
          !v <- PCG.uniformRI64 (0, range option) g :: IO Int64
          let op = ops !! snd (foldl (\(z,j) -> \x ->
                                       if (x+z>=p)
                                       then (x+z,j)
                                       else (x+z,j+1))
                               (0.0::Double, 0) [0.9999, 0.00005, 0.00005])
          op k v
          read_loop $ i - 1
          else return ()

--  putStrLn $ "Thread " ++ show tid ++ "started"
  !b <- waitBarrier bar
--  putStrLn $ "Thread id: " ++ show tid ++  ", seed: " ++ show seedn
  if b
    then do loop $ ops1 option
            let tran = ops !! 3
            tran 0 0
            read_loop $ ops2 option
            return 0
    else return 0

test :: Int -> [Int64 -> Int64 -> IO()] -> [Double] -> Flag -> PCG.GenIO -> IO (Double)
test !threadn !ops !ratios !option !gen = do
  performGC
  !bar <- newBarrier
  !asyncs <- mapM (\tid -> do
                       s <- PCG.uniformW64 gen :: IO Word64
                       async $ thread tid ops ratios option bar s) [1..threadn]
  signalBarrier bar True
  !start <- getCurrentTime
  !res <- mapM wait asyncs
  !end <- getCurrentTime
--  putStrLn $ show res
  return ((realToFrac $ diffUTCTime end start) * 1000.0 ::Double)

mean :: [Double] -> Double
mean xs = (sum xs) / ((realToFrac $ length xs) :: Double)

stddev :: [Double] -> Double
stddev xs = sqrt $ (sum (map (\x -> (x - avg) * (x - avg)) xs)) / len
  where len = (realToFrac $ length xs) :: Double
        avg = mean xs

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
       loop !i | i == end  = return ()
                  | otherwise = do fn i; loop (i+1)

run :: Int -> Flag -> IO()
run threadn option = do
  outh <- openFile ((file option) ++ "_" ++ (bench option) ++ ".csv") AppendMode
--  hPutStrLn outh "threadn,Mean,Stddev"
--  setStdGen $ mkStdGen $ seed option
  !gen <- PCG.restore $ PCG.initFrozen $ seed option
  let ratios = [gratio option, iratio option, 1.0 - (gratio option) - (iratio option)]

--  let size = 100000
--  let size = 1000 * 1000
  let size = fromIntegral (initial option) :: Int64
  let loop !i | i>0 = do
                 !ops <- case bench option of
                   "pure" -> do
                     !m <- PM.newMap
                     mapM_ (\k -> PM.ins k (k + 1) m) [1..size]
                     test threadn [(\k _ -> do !r <- PM.get k m ; return ()),
                                   (\k v -> PM.ins k v m),
                                   (\k _ -> PM.del k m)] ratios option gen
                   "cpure" -> do -- Compact Pure.
                     !mp <- PM.newMap
                     mapM_ (\k -> PM.ins k (k + 1) mp) [1..size]
                     mp2 <- PM.snapshot mp
                     !m <- CPM.fromMap mp2
                     -- mapM_ (\k -> CPM.ins k (k + 1) m) [1..size]
                     test threadn [(\k _ -> do !r <- CPM.get k m ; return ()),
                                   (\k v -> CPM.ins k v m),
                                   (\k _ -> CPM.del k m)] ratios option gen
                   "pureL" -> do
                     !m <- PML.newMap
                     mapM_ (\k -> PML.ins k (k + 1) m) [1..size]
                     test threadn [(\k _ -> do !r <- PML.get k m ; return ()),
                                   (\k v -> PML.ins k v m),
                                   (\k _ -> PML.del k m),
                                   nop] ratios option gen
                   "ctrie" -> do
                     !m <- CM.empty
                     mapM_ (\k -> CM.insert k (k + 1) m) [1..size]
                     test threadn [(\k _ -> do !r <- CM.lookup k m ; return ()),
                                   (\k v -> CM.insert k v m),
                                   (\k _ -> CM.delete k m),
                                   nop] ratios option gen
                   "adaptive" -> do
                     !m <- AM.newMap
                     mapM_ (\k -> AM.ins k (k + 1) m) [1..size]
                     test threadn [(\k _ -> do !r <- AM.get k m ; return ()),
                                   (\k v -> AM.ins k v m),
                                   (\k _ -> AM.del k m),
                                   (\_ _ -> AM.transition m)] ratios option gen
                   "nop" -> test threadn [nop, nop, nop, nop] ratios option gen
                   _ -> undefined
                 putStrLn $ show threadn ++ " threads: " ++ show ops ++ " ms"
                 hFlush stdout
                 !xs <- (loop $ i-1)
                 return $ ops : xs
                   where nop _ _ = do return ()
      loop _ = return []

  !xs <- loop $ (runs option) + (warmup option)
  let res = drop (warmup option) xs
  hPutStrLn outh $ (show threadn) ++ "," ++ (show $ mean res) ++ "," ++ (show $ stddev res)
  hClose outh

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number: " ++ show threadn
  putStrLn $ "Duration:      " ++ show (duration option) ++ "ms"
  putStrLn $ "Get Ratio:     " ++ show (gratio option)
  putStrLn $ "Delete Ratio:  " ++ show (1.0 - (gratio option) - (iratio option))
  putStrLn $ "Insert Ratio:  " ++ show (iratio option)
  putStrLn $ "Initial size:  " ++ show (initial option)
  putStrLn $ "Number of runs:" ++ show (runs option)
  putStrLn $ "Warmup runs:   " ++ show (warmup option)
  putStrLn $ "Seed:          " ++ show (seed option)
  putStrLn $ "File:          " ++ show (file option)
  putStrLn $ "OPS1:          " ++ show (ops1 option)
  putStrLn $ "OPS2:          " ++ show (ops2 option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={nop, pure, cpure, pureL, ctrie, adaptive})"
    else run threadn option
  return ()


data Flag =
       Flag
         { duration :: Int
         , gratio   :: Double
         , iratio   :: Double
         , initial  :: Int
         , seed     :: Word64
         , file     :: String
         , runs     :: Int
         , warmup   :: Int
         , bench    :: String
         , range    :: Int64
         , ops1     :: Int
         , ops2     :: Int
         }
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag
  { duration = 100 &= help "Duration"
  , warmup = 5 &= help "number of warmup run"
  , gratio = 1.0 / 3.0 &= help "Get Ratio"
  , iratio = 1.0 / 3.0 &= help "Insert ratio"
  , initial = 100000 &= help "Initial size"
  , range = 10000 &= help "Key range"
  , seed = 4096 &= help "Seed"
  , file = "report" &= help "Report file prefix"
  , runs = 25 &= help "Number of runs"
  , ops1 = 10000 &= help "ops before transition"
  , ops2 = 1000000 &= help "ops after transition"
  , bench = "" &= help "Benchvariant {nop, pure, cpure, pureL, ctrie, adaptive}"
  }
