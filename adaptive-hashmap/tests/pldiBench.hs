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
import GHC.Stats
import qualified System.Random.PCG.Fast.Pure as PCG
import Data.Int
import Data.Concurrent.DB
import qualified Data.Concurrent.DBctrie as DBC
import qualified Data.Concurrent.DBgz as DBZ
import System.Directory
import Prelude hiding (lookup, readFile)
import Control.DeepSeq
import System.Mem
import Data.IORef
import System.IO.Posix.MMap
import Data.Word

-- import Codec.Compression.QuickLZ
  
test :: DB m => (Int,Int) -> PCG.GenIO -> m -> [String] -> Flag -> IO ()
test (tid,thn) rng m files opt =
    let numFiles = length files in
  -- for_ 1 ((ops opt) `div` thn)
  --       (\_ -> 
           forM_ (zip [0..] files)
                 (\(ix,f) -> do
                     !s <- readFile $ (dir opt) ++ "/" ++ f
--                     s <- unsafeMMapFile $ (dir opt) ++ "/" ++ f
--          evaluate (bsSum s)
--                           putStrLn$ "Read file, num bytes: "++ show (B.length s)
--                     i <- PCG.uniform rng
                     insert (tid * numFiles + ix) s m
--                     performMajorGC -- After compression, GC.
                 )

newtype LSBS = LSBS (IORef [ByteString])
                                  
instance DB LSBS where
  -- insert :: Int -> ByteString -> a -> IO ()
  insert key bs (LSBS ref) = do
      modifyIORef' ref (\ls -> (bs:ls))
      return ()

----------------------------------------
-- Inclusive/Inclusive
for_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}
             
{-# INLINE rep #-}
rep :: Monad m => m a -> Int -> m ()
rep m n = for_ 1 n $ \_ -> m
----------------------------------------
             

checksum :: DB m => (Int,Int) -> PCG.GenIO -> m -> [String] -> Flag -> IO ()
checksum (tid,thn) rng m files opt = do
  let numFiles = length files 
  total <- newIORef (0::Int64)
  let myops = ((ops opt) `div` thn)
  putStrLn $ "Thread "++show tid++" issuing "++show myops++" lookup ops..."  
  for_ 1 myops
        (\_ -> do i <- PCG.uniformR (0,numFiles * thn - 1) rng
                  -- putStrLn $ "Looking up on key: "++show i
--                  putStr "."
                  Just bs <- lookup i m                  
                  sm <- evaluate (B.foldl' (+) 0 bs)
                  modifyIORef total (\s -> (s + fromIntegral (B.length bs)))
                  return ())
  t <- readIORef total
  putStrLn$ " ===============================> Total bytes read: "++show t 
  return ()          

             
benchmark :: DB m => Int -> PCG.GenIO -> [String] -> Flag
          -> IO m -> IO ()
benchmark thn rng files opt empty = do
  m <- empty
  stat1 <- getGCStats
  putStrLn$ "\n >> Starting write phase, filling memory.  Initial stats:\n "++show stat1
  !asyncs <- mapM (\tid -> do                       
                       !gen <- PCG.restore $ PCG.initFrozen (fromIntegral tid)
                       async $ test (tid,thn) gen m files opt)
                  [0..thn-1]
  !_res <- mapM wait asyncs
  stat2 <- getGCStats          
  putStrLn$ "\n >>> Switching all threads into read mode.  GC stats:\n " ++show stat2
           
  -- Read phase.. this had better not alloc, and had better not GC.
  !asyncs' <- mapM (\tid -> do                       
                       !gen <- PCG.restore $ PCG.initFrozen (fromIntegral tid)
                       async $ checksum (tid,thn) gen m files opt)
                  [0..thn-1]
  !_res <- mapM wait asyncs'
  stat3 <- getGCStats           
  putStrLn$ "\n >> Done with read phase.  Final stats:\n "++ show stat3
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

gcChatter :: IO ()
gcChatter = do
  stat1 <- getGCStats
  putStrLn$ "\n >> "++show stat1
  threadDelay (3000 * 1000)
         
main :: IO ()
main = do
  option <- cmdArgs $ flag
  threadn <- getNumCapabilities
  putStrLn $ "Thread number:  " ++ show threadn
  putStrLn $ "Seed:           " ++ show (seed option)
  putStrLn $ "Input Directory:" ++ show (dir option)
  putStrLn $ "ops:           " ++ show (ops option)
  putStrLn $ "Variant: " ++ show (bench option)

  forkIO gcChatter
           
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
