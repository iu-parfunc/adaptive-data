{-# LANGUAGE BangPatterns #-}

module Main
       where

import Control.Concurrent
import Control.Concurrent.Async (wait, withAsyncOn)
import Control.Monad
import Criterion.Main
import Criterion.Types
import Data.Atomics
import Data.Atomics.Vector
import Data.Int
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import System.Environment (getEnvironment, getArgs, withArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.Random

import qualified Data.Concurrent.PureQueue as PQ
import qualified Data.Concurrent.Queue.MichaelScott as MS
import qualified Data.Concurrent.AdaptiveBag as AB
import qualified Data.Concurrent.PureBag as PB
import qualified Data.Concurrent.OldBag as OB
import qualified Data.Concurrent.ScalableBag as SB

--------------------------------------------------------------------------------
-- Queue benchmarks:

fillN :: IO a -> (a -> Int64 -> IO ()) -> Int64 -> IO ()
fillN newQ insert num = do 
  q <- newQ
  for_ 1 num $ \i -> insert q i

forkNFill :: IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> IO ()
forkNFill newQ insert elems splits = do
  q <- newQ
  let quota = fromIntegral $ elems `quot` splits
  -- putStrLn$ "Splitting into this many ops per thread : "++show quota
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota - 1) $
                        \i -> insert q i)

--------------------------------------------------------------------------------
-- Bag benchmarks:

-- Producer/consumer version with half producers half consumers:
pushPopN :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int64 -> IO ()
pushPopN newBag push pop total = do
  bag <- newBag
  for_ 1 total $ \i -> do
    if even i then void $ pop bag else push bag i

{-
forkNFillBag ::  IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> IO ()
forkNFillBag newBag insert elems splits = do
  q <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota - 1) $
                        \i -> insert q i)
-}

forkNPushPop :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int -> Int -> IO ()
forkNPushPop newBag push pop elems splits = do
  bag <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      --  Interleave pushes and pops
                      for_ offset (offset + quota - 1) $ \i ->
                        if even chunk then void $ pop bag
                        else push bag i)

fork5050 :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int -> Int -> VS.Vector Int -> IO ()
fork5050 newBag push pop elems splits vec = do
  bag <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let shouldPop = vec VS.! chunk == 0
                          offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota - 1) $ \i ->
                        if shouldPop then void $ pop bag
                        else push bag i)

hotKeyOrRandom :: IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> VM.IOVector (Maybe a) -> IO ()
hotKeyOrRandom newBag push reps splits vec = do
  let quota = (fromIntegral reps) `quot` splits
  forkJoin splits (\chunk -> for_ 1 (fromIntegral quota) $ \i -> do
                      flip <- randomRIO (0, 1) :: IO Float
                      idx <- if flip < 0.5 then randomRIO (0, VM.length vec - 1) else return 0
                      tick <- readVectorElem vec idx
                      b <- case peekTicket tick of
                        Nothing -> do
                          b <- newBag
                          (success, tick') <- casVectorElem vec idx tick $ Just b
                          if success
                            then return b else case peekTicket tick' of
                            Nothing -> error "shouldn't see this"
                            Just b' -> return b'
                        Just b -> return b
                      push b i)

main :: IO ()
main = do
  splits <- getNumCapabilities
  -- Initialize randomness for fork5050
  randomVec <- VS.replicateM splits (randomRIO (0, 1) :: IO Int)
  -- Initialize vector of Nothing for hotKeyOrRandom
  let vecSize = getNumEnvVar 100 "VEC_SIZE"
  pureNothingVec <- VM.replicate vecSize Nothing -- TODO how big should this vector be?
  scalableNothingVec <- VM.replicate vecSize Nothing
  adaptiveNothingVec <- VM.replicate vecSize Nothing
  let adaptThresh = getNumEnvVar 10 "ADAPT_THRESH"
  putStrLn $ "[benchmark] using " ++ show splits ++ " capabilities"
  args <- getArgs
  -- when (null args) (error "Expected at least one command line arg: pure, scalable, hybrid")
-- RRN: disabling queues for now.  We're off queues.
{-    
    bgroup "PureQueue" [
       bench "new" $ Benchmarkable $ rep PQ.newQ,
       bgroup "single-threaded" [
          bench ("push-"++show n) $ Benchmarkable $ rep (fillN PQ.newQ PQ.pushL n)
          | n <- sizes],
       bgroup "multi-threaded" [
          bench ("push-"++show elems) $ Benchmarkable $ rep (forkNFill PQ.newQ PQ.pushL elems splits)
          | elems <- parSizes]
       ],
    bgroup "LinkedQueue" [
      bench "new" $ Benchmarkable $ rep MS.newQ,
       bgroup "single-threaded" [
          bench ("push-"++show n) $ Benchmarkable $ rep (fillN MS.newQ MS.pushL n)
          | n <- sizes],
       bgroup "multi-threaded" [
          bench ("push-"++show elems) $ Benchmarkable $ rep (forkNFill MS.newQ MS.pushL elems splits)
          | elems <- parSizes]
      ],
-}
  let pure =
        [ bench "bag_new-1" $ Benchmarkable $ rep PB.newBag ] ++
        [ bench ("bag_insert-" ++ show elems) $
          Benchmarkable $ rep (forkNFill PB.newBag PB.add elems splits)
        | elems <- parSizes ] ++ 
        [ bench ("bag_random5050-" ++ show elems) $
          Benchmarkable $ rep (fork5050 PB.newBag PB.add PB.remove elems splits randomVec)
        | elems <- parSizes ] ++
        [ bench ("array-bag_hotcold-insert-"++ show hotkeySize) $
          Benchmarkable $ rep (hotKeyOrRandom PB.newBag PB.add hotkeySize splits pureNothingVec) ]

      oldpure =
        [ bench "bag_new-1" $ Benchmarkable $ rep OB.newBag ] ++
        [ bench ("bag_insert-" ++ show elems) $
          Benchmarkable $ rep (forkNFill OB.newBag OB.add elems splits)
        | elems <- parSizes ] ++ 
        [ bench ("bag_random5050-" ++ show elems) $
          Benchmarkable $ rep (fork5050 OB.newBag OB.add OB.remove elems splits randomVec)
        | elems <- parSizes ] ++
        [ bench ("array-bag_hotcold-insert-"++ show hotkeySize) $
          Benchmarkable $ rep (hotKeyOrRandom OB.newBag OB.add hotkeySize splits pureNothingVec) ]      
      
      scalable =
        [ bench "bag_new-1" $ Benchmarkable $ rep SB.newBag ]  ++
        [ bench ("bag_insert-" ++ show elems) $
          Benchmarkable $ rep (forkNFill SB.newBag SB.add elems splits)
        | elems <- parSizes ] ++ 
        [ bench ("bag_random-" ++ show elems) $
          Benchmarkable $ rep (fork5050 SB.newBag SB.add SB.remove elems splits randomVec)
        | elems <- parSizes
        ] ++
        [ bench ("array-bag_hotcold-insert-0.5-"++show hotkeySize) $
          Benchmarkable $ rep (hotKeyOrRandom SB.newBag SB.add hotkeySize splits scalableNothingVec) ]

  -- TODO: FURTHER DEDUPLICATE these three blocks of code!
      hybrid = 
        [ bench "bag_new-1" $ Benchmarkable $ rep $ AB.newBagThreshold adaptThresh ] ++
        [ bench ("bag_insert-" ++ show elems) $
          Benchmarkable $ rep (forkNFill AB.newBag AB.add elems splits)
        | elems <- parSizes ] ++         
        [ bench ("bag_random-" ++ show elems) $
          Benchmarkable $ rep (fork5050 (AB.newBagThreshold adaptThresh) AB.add AB.remove elems splits randomVec)
        | elems <- parSizes ] ++
        [ bench ("array-bag_hotcold-insert-0.5-"++show hotkeySize) $ 
          Benchmarkable $ rep
          (hotKeyOrRandom (AB.newBagThreshold adaptThresh) AB.add hotkeySize splits adaptiveNothingVec) ]
         
   -- Hack to make the names come out right in the upload:
  let (args',list) =
       case args of
       "pure":t     -> (t,pure)
       "oldpure":t  -> (t,oldpure)
       "scalable":t -> (t,scalable)
       "hybrid":t   -> (t,hybrid)
       t            -> (t,[ bgroup "pure"     pure,
                            bgroup "oldpure"  oldpure,
                            bgroup "scalable" scalable,
                            bgroup "hybrid"   hybrid ])

  withArgs args' $ defaultMain list

  where sizes = [10^e | e <- [0..4]]
        parSizes = [ 10000, 100000, 500000 ]
        hotkeySize = 100000

for_ :: Monad m => Int64 -> Int64 -> (Int64 -> m a) -> m ()
for_ start end _ | start > end = error "start greater than end"
for_ start end fn = loop start
  where loop !i | i > end = return ()
                | otherwise = fn i >> loop (i+1)
{-# INLINE for_ #-}

getNumEnvVar :: Int -> String -> Int
getNumEnvVar deflt name =
  case lookup name theEnv of
    Nothing -> deflt
    Just s -> read s

{-# NOINLINE theEnv #-}
theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

-- | Run N copies of an IO action in parallel. Pass in a number from
-- 0..N-1, letting the worker know which it is.
forkJoin :: Int -> (Int -> IO ()) -> IO ()
forkJoin num act = loop2 num []
  where
    -- VERSION 2: The less safe version:
    loop2 0 ls = mapM_ takeMVar ls
    loop2 n ls = do mv <- newEmptyMVar
                    _ <- forkOn (n-1) (do act (n-1); putMVar mv ())
                    loop2 (n-1) (mv:ls)

{-# INLINE rep #-}
rep :: Monad m => m a -> Int64 -> m ()
rep m n = for_ 1 n $ \_ -> m
