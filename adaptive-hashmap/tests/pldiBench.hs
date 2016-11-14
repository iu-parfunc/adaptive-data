{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString, readFile)
import System.Console.CmdArgs hiding (opt)
import System.IO hiding (readFile)
import Data.Time.Clock
import Data.List (findIndex)
import qualified Data.Vector as V
import GHC.Word
import qualified System.Random.PCG.Fast.Pure as PCG
import Data.Int
import Data.Concurrent.DB
import qualified Data.Concurrent.DBctrie as DBC
import qualified Data.Concurrent.DBgz as DBZ
import qualified Data.Concurrent.DBadaptive as DBA
import qualified Data.Concurrent.DBagz as DBAZ
import System.Directory
import Prelude hiding (lookup, readFile)
import Control.DeepSeq

chooseFile :: PCG.GenIO -> String -> [String] -> IO ByteString
chooseFile !rng !datadir !files = do
  !n <- PCG.uniformB (length files) rng
  !s <- readFile $ datadir ++ "/" ++ (files !! n)
  return $ s

chooseOp :: Double -> [() -> IO ()] -> [Double] -> Bool -> IO (() -> IO ())
chooseOp !r !op !prob !isHot = do
  let op' = if isHot then op else reverse op
  return $ case findIndex (\p -> r <= p) prob of
             Just i -> op' !! i
             Nothing -> undefined

createOp :: DB m => PCG.GenIO -> Flag -> m -> [String]
         -> [Double] -> Bool -> IO (() -> IO ())
createOp !rng !opt !m !files !prob !isHot = do
  !r <- PCG.uniformBD 1.0 rng
  !k <- PCG.uniformB (range opt) rng
  !s <- if r <= iratio opt || (not isHot)
        then chooseFile rng (dir opt) files
        else return ""
  let oplst = [(\_ -> do
                   !v <- insert (fromIntegral k) s m
                   deepseq v $ return ())
            -- ,(\_ -> do
            --      !v <- delete (fromIntegral k) m
            --      deepseq v $ return ())
              ,(\_ -> do
                   !v <- lookup (fromIntegral k) m
                   deepseq v $ return ())]
  chooseOp r oplst prob isHot

performOp :: DB m => Int -> PCG.GenIO -> V.Vector m -> Flag -> [String]
          -> [Double] -> IO Double
performOp !n !rng !vec !opt !files !prob = do
  !r <- PCG.uniformBD 1.0 rng
  !rn <- PCG.uniformB ((V.length vec) - 1) rng
  let isHot = r <= 0.8
  let m = if isHot
          then vec V.! n
          else vec V.! (if rn >= n then rn + 1 else rn)
  op <- createOp rng opt m files prob isHot
  measure op

measure :: (() -> IO ()) -> IO Double
measure op = do
  !start <- getCurrentTime
  op ()
  !end <- getCurrentTime
  return $ (realToFrac $ diffUTCTime end start) * 1000.0

unitOps :: DB m => Int -> PCG.GenIO -> V.Vector m -> Flag -> [String]
        -> [Double] -> Bool -> IO Double
unitOps !n !rng !vec !opt !files !prob !tran = do
  s <- loop 0 0.0
  return $ s / (fromIntegral (unit opt) + (if tran then 1 else 0))
  where
    loop i acc = do
      if i < unit opt
        then do l <- performOp n rng vec opt files prob
                loop (i + 1) (acc + l)
        else if tran
             then do l <- measure (\_ -> transition $ vec V.! n)
                     return $ acc + l
             else return acc

phase :: DB m => Int -> PCG.GenIO -> V.Vector m -> Flag -> [String]
      -> [Double] -> IO (V.Vector Double)
phase !n !rng !vec !opt !files !prob = do
  putStrLn $ "phase: " ++ show n
  hFlush stdout
  loop V.empty 0
  where
    len = ((ops opt) `div` (unit opt))
    loop v i = do
      if i < len
      then do m <- unitOps n rng vec opt files prob (i < len - 1)
              loop (V.snoc v m) (i + 1)
      else return v

thread :: DB m => Int -> Flag -> [String] -> V.Vector m -> Word64 -> IO (V.Vector Double)
thread _ opt files vec initseed = do
  let prob = [(iratio opt), 1.0]
  !rng <- PCG.restore $ PCG.initFrozen $ initseed
  foldM (\v n -> do
            v' <- phase n rng vec opt files prob
            return $ v V.++ v')
        V.empty [0..((nDB opt) - 1)]
  
initDB :: DB m => IO m -> Int -> IO (V.Vector m)
initDB empty n = do
  lst <- sequence $ map (\_ -> empty) [1..n]
  return $ V.fromList lst

mean :: [Double] -> Double
mean xs = (sum xs) / ((realToFrac $ length xs) :: Double)

benchmark :: DB m => Int -> PCG.GenIO -> [String] -> Flag
          -> IO m -> Handle -> IO ()
benchmark thn rng files opt empty out = do
  vec <- initDB empty (nDB opt)
  !asyncs <- mapM (\tid -> do
                       s <- PCG.uniformW64 rng
                       async $ thread tid opt files vec s)
                  [1..thn]
  !res <- mapM wait asyncs
  let len = (nDB opt) * ((ops opt) `div` (unit opt))
  forM_ [0..(len - 1)]
        (\i -> do
            let !m = mean $ map (\v -> v V.! i) res
            hPutStrLn out $ show i ++ "," ++ show m)
  hClose out
  return ()

run :: Int -> Flag -> IO ()
run thn opt = do
  !gen <- PCG.restore $ PCG.initFrozen $ seed opt
  let fileName = (file opt) ++ "_" ++ (bench opt)
  outh <- openFile (fileName ++ ".csv") WriteMode
  lst <- getDirectoryContents $ dir opt
  let files = drop 2 lst
  case (bench opt) of
    "ctrie" -> benchmark thn gen files opt DBC.empty outh
    "gz" -> benchmark thn gen files opt DBZ.empty outh
    "adaptive" -> benchmark thn gen files opt DBA.empty outh
    "agz" -> benchmark thn gen files opt DBAZ.empty outh
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
  putStrLn $ "Seed:           " ++ show (seed option)
  putStrLn $ "Report File:    " ++ show (file option)
  putStrLn $ "Input Directory:" ++ show (dir option)
  putStrLn $ "unit:           " ++ show (unit option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={gz, ctrie, adaptive, agz})"
    else 
      if length (dir option) == 0
        then putStrLn $ "Need to specify input directory. (By --dir)"
        else run threadn option

  return ()

data Flag
  = Flag {nDB :: Int,
          iratio :: Double,
          seed :: Word64,
          file :: String,
          bench :: String,
          range :: Int64,
          ops :: Int,
          dir :: String,
          unit :: Int}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {nDB = 10 &= help "number of DBs",
             iratio = 0.8 &= help "Insert ratio",
             range = 65536 &= help "Key range",
             seed = 8192 &= help "Seed",
             file = "report" &= help "Report file prefix",
             ops = 10000 &= help "number of operations per phase",
             dir = "" &= help "input file directory",
             unit = 100 &= help "record average of every unit ops",
             bench = "" &= help "Benchvariant {ctrie, gz, adaptive}"}
