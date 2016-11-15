{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Console.CmdArgs hiding (opt)
import System.IO hiding (readFile)
import Data.Time.Clock
import Data.List (findIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import GHC.Word
import qualified System.Random.PCG.Fast.Pure as PCG
import Data.Int
import Data.Concurrent.Set
import qualified Data.Concurrent.SetCtrie as DSC
import qualified Data.Concurrent.SetBit as DSB
import qualified Data.Concurrent.SetAdaptive as DSA
import Control.DeepSeq
import Data.Atomics.Counter
import GHC.Conc.Sync

chooseOp :: Double -> [() -> IO ()] -> [Double] -> Bool -> IO (() -> IO ())
chooseOp !r !op !prob !isHot = do
  let op' = if isHot then op else reverse op
  return $ case findIndex (\p -> r <= p) prob of
             Just i -> op' !! i
             Nothing -> undefined

createOp :: DSet m => PCG.GenIO -> Flag -> m
         -> [Double] -> Bool -> IO (() -> IO ())
createOp !rng !opt !m !prob !isHot = do
  !r <- PCG.uniformBD 1.0 rng
  !k <- PCG.uniformB (range opt) rng
  let oplst = [(\_ -> do
                   !v <- insert (fromIntegral k) m
                   deepseq v $ return ())
              ,(\_ -> do
                   !v <- delete (fromIntegral k) m
                   deepseq v $ return ())
              ,(\_ -> do
                   !v <- member (fromIntegral k) m
                   deepseq v $ return ())]
  chooseOp r oplst prob isHot

performOp :: DSet m => Int -> PCG.GenIO -> V.Vector m -> Flag
          -> [Double] -> IO Double
performOp !n !rng !vec !opt !prob = do
  !r <- PCG.uniformBD 1.0 rng
  !rn <- PCG.uniformB ((V.length vec) - 1) rng
  let isHot = r <= (hratio opt)
  let m = if isHot
          then vec V.! n
          else vec V.! (if rn >= n then rn + 1 else rn)
  op <- createOp rng opt m prob isHot
  measure op

measure :: (() -> IO ()) -> IO Double
measure op = do
  !start <- getCurrentTime
  op ()
  !end <- getCurrentTime
  return $ (realToFrac $ diffUTCTime end start) * 1000.0

unitOps :: DSet m => Int -> PCG.GenIO -> V.Vector m -> Flag
        -> [Double] -> IO Double
unitOps !n !rng !vec !opt !prob = do
  s <- loop 0 0.0
  return $ s / (fromIntegral (unit opt))
  where
    loop i acc = do
      if i < unit opt
        then do l <- performOp n rng vec opt prob
                loop (i + 1) (acc + l)
        else return acc

phase :: DSet m => Int -> PCG.GenIO -> V.Vector m -> Flag
      -> [Double] -> V.Vector AtomicCounter -> VM.IOVector Double
      -> Int -> IO ()
phase !n !rng !vec !opt !prob !ac !v !offset = do
--  putStrLn $ "phase: " ++ show n
--  hFlush stdout
  loop 0
  where
    len = ((ops opt) `div` (unit opt))
    loop i = do
      if i < len
        then do m <- unitOps n rng vec opt prob
                VM.write v (i + offset) m
                loop (i + 1)
        else do n' <- incrCounter 1 $ ac V.! n
                if n' == numCapabilities
                  then do putStrLn $ "phase " ++ show n
                          hFlush stdout
                          transition $ vec V.! n
                  else return ()

thread :: DSet m => Int -> Flag -> V.Vector m -> Word64
       -> V.Vector AtomicCounter -> IO (VM.IOVector Double)
thread _ opt vec initseed ac = do
  let prob = [(iratio opt), 1.0]
      len  = ((ops opt) `div` (unit opt))
  !rng <- PCG.restore $ PCG.initFrozen $ initseed
  !v <- VM.new $ (nDB opt) * len
  foldM_ (\_ n -> do
             phase n rng vec opt prob ac v (n * len))
         () [0..((nDB opt) - 1)]
  return v
  
initDB :: DSet m => IO m -> Int -> IO (V.Vector m)
initDB empty n = do
  lst <- sequence $ map (\_ -> empty) [1..n]
  return $ V.fromList lst

initAC :: Int -> IO (V.Vector AtomicCounter)
initAC n = do
  lst <- sequence $ map (\_ -> newCounter 0) [1..n]
  return $ V.fromList lst

mean :: [Double] -> Double
mean xs = (sum xs) / ((realToFrac $ length xs) :: Double)

benchmark :: DSet m => Int -> PCG.GenIO -> Flag
          -> IO m -> Handle -> IO ()
benchmark thn rng opt empty out = do
  vec <- initDB empty (nDB opt)
  ac  <- initAC (nDB opt)
  !asyncs <- mapM (\tid -> do
                       s <- PCG.uniformW64 rng
                       async $ thread tid opt vec s ac)
                  [1..thn]
  !res <- mapM wait asyncs
  let len = (nDB opt) * ((ops opt) `div` (unit opt))
  putStrLn "Collect results"
  forM_ [0..(len - 1)]
        (\i -> do
            !m <- sequence $ map (\v -> VM.read v i) res 
            hPutStrLn out $ show i ++ "," ++ (show $ mean m))
  hClose out
  return ()

run :: Int -> Flag -> IO ()
run thn opt = do
  !gen <- PCG.restore $ PCG.initFrozen $ seed opt
  let fileName = (file opt) ++ "_" ++ (bench opt)
  outh <- openFile (fileName ++ ".csv") WriteMode
  case (bench opt) of
    "ctrie" -> benchmark thn gen opt DSC.empty outh
    "bset" -> benchmark thn gen opt DSB.empty outh
    "adaptive" -> benchmark thn gen opt DSA.empty outh
    _ -> undefined
  return ()

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number:  " ++ show threadn
  putStrLn $ "number of DB:   " ++ show (nDB option)
  putStrLn $ "number of ops:  " ++ show (ops option)
  putStrLn $ "Get Ratio:      " ++ show (1.0 - iratio option)
  -- putStrLn $ "Delete Ratio:   " ++ show (1.0 - (gratio option) - (iratio option))
  putStrLn $ "Insert Ratio:   " ++ show (iratio option)
  putStrLn $ "Hot Ratio:      " ++ show (hratio option)
  putStrLn $ "Seed:           " ++ show (seed option)
  putStrLn $ "Report File:    " ++ show (file option)
  putStrLn $ "unit:           " ++ show (unit option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={ctrie, bset, adaptive})"
    else run threadn option

  return ()

data Flag
  = Flag {nDB :: Int,
          iratio :: Double,
          hratio :: Double,
          seed :: Word64,
          file :: String,
          bench :: String,
          range :: Int64,
          ops :: Int,
          unit :: Int}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {nDB = 10 &= help "number of DBs",
             iratio = 0.8 &= help "Insert ratio",
             hratio = 0.8 &= help "Hot ratio",
             range = 65536 &= help "Key range",
             seed = 8192 &= help "Seed",
             file = "report" &= help "Report file prefix",
             ops = 10000 &= help "number of operations per phase",
             unit = 100 &= help "record average of every unit ops",
             bench = "" &= help "Benchvariant {ctrie, gz, adaptive}"}
