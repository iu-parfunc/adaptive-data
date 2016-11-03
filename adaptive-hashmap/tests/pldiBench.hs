{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Extra
import System.Console.CmdArgs
import System.IO
import System.Mem
import Data.Time.Clock
import qualified Data.Vector as V
import GHC.Word
import qualified System.Random.PCG.Fast.Pure as PCG
import Data.Int
import Data.Concurrent.DB (DB)
import Data.Concurrent.DBctrie as DBC
import Data.Concurrent.DBgz as DBZ
import System.Directory 

performOp :: DB m => PCG.GenIO -> m -> IO ()
performOp m r = undefined

run :: Int -> Flag -> IO ()
run thn opt = do
  !gen <- PCG.restore $ PCG.initFrozen $ seed opt
  let fileName = (file opt) ++ "_" ++ (bench opt)
--  outh <- openFile (fileName ++ ".csv") AppendMode
  lst <- getDirectoryContents $ dir opt
  let files = drop 2 lst
  let lstDB = map (\_ -> do
                      !m <- case (bench opt) of
                        "ctrie" -> DBC.empty
                        "gz" -> DBZ.empty
                      return m)
                  [1..(nDB opt)]
  let vec = V.fromList lstDB
  return ()

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number:  " ++ show threadn
  putStrLn $ "number of DB:   " ++ show (nDB option)
  putStrLn $ "number of ops:  " ++ show (ops option)
  putStrLn $ "Get Ratio:      " ++ show (gratio option)
  putStrLn $ "Delete Ratio:   " ++ show (1.0 - (gratio option) - (iratio option))
  putStrLn $ "Insert Ratio:   " ++ show (iratio option)
  putStrLn $ "Seed:           " ++ show (seed option)
  putStrLn $ "Report File:    " ++ show (file option)
  putStrLn $ "Input Directory:" ++ show (dir option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={gz, ctrie, adaptive})"
    else 
      if length (dir option) == 0
        then putStrLn $ "Need to specify input directory. (By --dir)"
        else run threadn option

  return ()

data Flag
  = Flag {nDB :: Int,
          gratio :: Double,
          iratio :: Double,
          seed :: Word64,
          file :: String,
          bench :: String,
          range :: Int64,
          ops :: Int,
          dir :: String}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {nDB = 32 &= help "number of DBs",
             gratio = 0.25 &= help "Get Ratio",
             iratio = 0.5 &= help "Insert ratio",
             range = 65536 &= help "Key range",
             seed = 8192 &= help "Seed",
             file = "report" &= help "Report file prefix",
             ops = 10000 &= help "number of operations per phase",
             dir = "" &= help "input file directory",
             bench = "" &= help "Benchvariant {ctrie, gz, adaptive}"}
