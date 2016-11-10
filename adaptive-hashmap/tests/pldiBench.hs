{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import Control.Concurrent
import Control.Exception
import Control.Concurrent.Async
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString (ByteString, readFile)
--import Data.ByteString.Lazy (ByteString, fromStrict)
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
import System.Directory
import Prelude hiding (lookup, readFile)
import Control.DeepSeq

import Data.IORef
import System.IO.Posix.MMap
import Data.Word

-- import Codec.Compression.QuickLZ
  
test :: DB m => Int -> PCG.GenIO -> m -> [String] -> Flag -> IO ()
test thn rng m files opt =
  forM_ [1..((ops opt) `div` thn)]
        (\_ -> 
           forM_ files
                 (\f -> do
                     !s <- readFile $ (dir opt) ++ "/" ++ f
--          s <- unsafeMMapFile $ (dir opt) ++ "/" ++ f
--          evaluate (bsSum s)
--                           putStrLn$ "Read file, num bytes: "++ show (B.length s)
                     i <- PCG.uniform rng
                     insert i s m))

newtype LSBS = LSBS (IORef [ByteString])
                                  
instance DB LSBS where
  -- insert :: Int -> ByteString -> a -> IO ()
  insert key bs (LSBS ref) = do
      modifyIORef' ref (\ls -> (bs:ls))
      return ()
                                  
benchmark :: DB m => Int -> PCG.GenIO -> [String] -> Flag
          -> IO m -> IO ()
benchmark thn rng files opt empty = do
  m <- empty
  !asyncs <- mapM (\tid -> do
                       !s <- PCG.uniformW64 rng
                       async $ test thn rng m files opt)
                  [1..thn]
  !res <- mapM wait asyncs
  return ()

run :: Int -> Flag -> IO ()
run thn opt = do
  !gen <- PCG.restore $ PCG.initFrozen $ seed opt
  lst <- getDirectoryContents $ dir opt
  let files = filter (/= "..") $ filter (/= ".") $ lst
  putStrLn $ "Number of files: "++show (length files)
  case (bench opt) of
    "ctrie" -> benchmark thn gen files opt DBC.empty
    "gz" -> benchmark thn gen files opt DBZ.empty
    "list" -> benchmark thn gen files opt (LSBS <$> newIORef [])
    _ -> undefined
  return ()

main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number:  " ++ show threadn
  putStrLn $ "Seed:           " ++ show (seed option)
  putStrLn $ "Input Directory:" ++ show (dir option)
  putStrLn $ "ops:           " ++ show (ops option)

  if length (bench option) == 0
    then putStrLn $ "Need to specify benchvariant. (By --bench={gz, ctrie, adaptive})"
    else 
      if length (dir option) == 0
        then putStrLn $ "Need to specify input directory. (By --dir)"
        else run threadn option

  return ()

----------------------------------------

bsSum bs = (B.foldr' (\ c acc -> acc + fromIntegral c) (0::Word64) bs)

-- -- Should use ~0 bytes
-- main :: IO ()
-- main = do
--   bs <- unsafeMMapFile "./data/fair.c"
--   -- putStrLn $ "Character number 100K: "++ show (B.index bs 100000)
--   putStrLn $ "Sum of chars "++ show (bsSum bs)
--   return ()

         
-- main :: IO ()
-- main = do
--   bs <- unsafeMMapFile "./data/fair.c"
--   putStrLn $ "Character number 100K: "++ show (B.index bs 100000)
--   return ()

         

         
         
data Flag
  = Flag {ops :: Int,
          seed :: Word64,
          dir :: String,
          bench :: String}
  deriving (Eq, Show, Data, Typeable)

flag :: Flag
flag = Flag {ops = 10 &= help "numbre of iteration",
             seed = 8192 &= help "Seed",
             dir = "" &= help "input file directory",
             bench = "" &= help "Benchvariant {ctrie, gz, adaptive}"}
