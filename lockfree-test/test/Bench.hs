{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
       where

import Control.Concurrent
-- import Control.Concurrent.Async (wait, withAsyncOn)
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

import qualified Data.TLS.PThread as TLS


-- import qualified Data.Concurrent.PureQueue as PQ
-- import qualified Data.Concurrent.Queue.MichaelScott as MS
import qualified Data.Concurrent.AdaptiveBag as AB
import qualified Data.Concurrent.PureBag as PB
import qualified Data.Concurrent.OldBag as OB
import qualified Data.Concurrent.ScalableBag as UB
import qualified Data.Concurrent.ScalableBagBoxed  as SB
import qualified Data.Concurrent.ScalableBagChaseLev as SBCL

--------------------------------------------------------------------------------
-- Queue benchmarks:

{-# INLINE fillN #-}
fillN :: IO a -> (a -> Int64 -> IO ()) -> Int64 -> IO ()
fillN newQ insert num = do 
  q <- newQ
  for_ 1 num $ \i -> insert q i

{-# INLINE forkNFill #-}
forkNFill :: IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> IO ()
forkNFill newQ insert elems splits = do
  q <- newQ
  let quota = fromIntegral $ elems `quot` splits
  -- putStrLn$ "Splitting into this many ops per thread : "++show quota
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota) $
                        \i -> insert q i)

--------------------------------------------------------------------------------
-- Bag benchmarks:

{-# INLINE pushPopN #-}
{-# INLINE forkNPushPop #-}
{-# INLINE fork5050 #-}
{-# INLINE hotKeyOrRandom #-}

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
                      for_ offset (offset + quota) $
                        \i -> insert q i)
-}

forkNPushPop :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int -> Int -> IO ()
forkNPushPop newBag push pop elems splits = do
  bag <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let offset = fromIntegral $ chunk * fromIntegral quota
                      --  Interleave pushes and pops
                      for_ offset (offset + quota) $ \i ->
                        if even chunk then void $ pop bag
                        else push bag i)

fork5050 :: IO a -> (a -> Int64 -> IO ()) -> (a -> IO (Maybe Int64)) -> Int -> Int -> VS.Vector Int -> IO ()
fork5050 newBag push pop elems splits vec = do
  bag <- newBag
  let quota = fromIntegral $ elems `quot` splits
  forkJoin splits (\chunk -> do
                      let shouldPop = vec VS.! chunk == 0
                          offset = fromIntegral $ chunk * fromIntegral quota
                      for_ offset (offset + quota) $ \i ->
                        if shouldPop then void $ pop bag
                        else push bag i)

hotKeyOrRandom :: forall a .
                  IO a -> (a -> Int64 -> IO ()) -> Int -> Int -> VM.IOVector (Maybe a) -> IO ()
hotKeyOrRandom newBag push reps splits vec = do
  let quota = (fromIntegral reps) `quot` splits
  forkJoin splits (\ _chunkID -> for_ 1 (fromIntegral quota) $ \i -> do
                      flp <- randomRIO (0, 1) :: IO Float
                      idx <- if flp < 0.5 then randomRIO (0, VM.length vec - 1) else return 0
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

  -- Initialize vector of Nothing for hotKeyOrRandom.  This is the
  -- outer collection of a nested collection.
  let vecSize = getNumEnvVar 100 "NUM_KEYS"

  -- FINISHME, use this!
  let _adaptThresh = getNumEnvVar 10 "ADAPT_THRESH"
      
  putStrLn $ "[benchmark] using " ++ show splits ++ " capabilities"
  putStr "[benchmark] OS thread ID of all current worker threads:\n  "
  forkJoin splits (\ix -> do tid <- TLS.getTLS SB.osThreadID
                             putStr $ show ix ++ ":" ++ show tid ++ "  ")
  putStrLn ""
  
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
  let mkBagBenchSet :: forall a0 . IO a0 -> (a0 -> Int64 -> IO ())
                    -> (a0 -> IO (Maybe Int64))
                    -> IO [Benchmark]
      mkBagBenchSet newBag add remove = do
       nothingVec <- VM.replicate vecSize Nothing -- TODO how big should this vector be?
       return $ 
        [ bench "bag_new-1" $ Benchmarkable $ rep newBag ] ++

        -- These benchmarks should be equivalent but measured differently:
        -- Doing BOTH just as a sanity check:
        ----------------------------------------
        -- This measures the marginal cost of one operation on ONE thread, under
        -- a varying amount of contention.        
        [ bench "bag_perthreadop-parfill-N" $ Benchmarkable $ \num -> do
           -- putStrLn $ "Forking "++show splits++" threads to each insert "++show num++" elements"
           -- forkJoin splits (\_ -> fillN newBag add num)
           forkNFill newBag add (fromIntegral num * splits) splits
        ] ++

        -- This one measures something funny: the marginal cost of
        -- adding one work item to a pool which is completed by N
        -- separate workers.  This number should go down as N increases.
        [ bench "bag_team-parfill-N" $ Benchmarkable $ \num -> do
           -- let quota = num `quot` fromIntegral splits
           -- putStrLn $ "Forking "++show splits++" threads to each insert "++show quota++" elements"
           -- forkJoin splits (\_ -> fillN newBag add quota)
           forkNFill newBag add (fromIntegral num) splits
        ] ++
        ----------------------------------------
  
        {- [ bench ("bag_insert-" ++ show elems) $
          Benchmarkable $ rep (forkNFill newBag add elems splits)
        | elems <- parSizes ] ++  -}
        [ bench ("bag_random5050-" ++ show elems) $
          Benchmarkable $ rep (fork5050 newBag add remove elems splits randomVec)
        | elems <- parSizes ] ++

        [ bench ("array-bag_hotcold-team-fill-N") $ Benchmarkable $ \num -> 
          (hotKeyOrRandom newBag add (fromIntegral num) splits nothingVec) ] ++ 
        [ bench ("array-bag_hotcold-insert-"++ show hotkeySize) $
          Benchmarkable $ rep (hotKeyOrRandom newBag add hotkeySize splits nothingVec) ] 

  
  pure     <- mkBagBenchSet PB.newBag PB.add PB.remove
  oldpure  <- mkBagBenchSet OB.newBag OB.add OB.remove
  scalable <- mkBagBenchSet SB.newBag SB.add SB.remove
  hybrid   <- mkBagBenchSet AB.newBag AB.add AB.remove

  scalableUB <- mkBagBenchSet UB.newBag UB.add UB.remove
  scalableCL <- mkBagBenchSet SBCL.newBag SBCL.add SBCL.remove

  -- These are not specific to an implementation:
  let basicBenchmarks =
        [ bench ("emptyForkJoin") (whnfIO $ forkJoin splits (\_ -> return ()))
          --        | num <- [1..splits] 
        , bench ("getOSTID") $ whnfIO $ TLS.getTLS SB.osThreadID
        , bench ("forkJoin-getOSTID-N") $ Benchmarkable $ \n ->
           forkJoin splits (\_ -> rep (TLS.getTLS SB.osThreadID) n)
           -- Sanity check about scalability:
        , bench ("team-separate-bags") $ Benchmarkable $ \ num ->
          let quota = num `quot` fromIntegral splits in 
          forkJoin splits (\_ -> do bg <- PB.newBag
                                    for_ 1 quota (PB.add bg))
        ]

   -- Hack to make the names come out right in the upload:
  let (args',list) =
       case args of
       "pure":t     -> (t,pure ++ basicBenchmarks)
       "oldpure":t  -> (t,oldpure)
       "scalable":t -> (t,scalable)
       "scalable-chaselev":t -> (t,scalableCL)
       "hybrid":t   -> (t,hybrid)
       t            -> (t,[ bgroup "pure"     pure,
                            bgroup "oldpure"  oldpure,
                            bgroup "scalable" scalable,
                            bgroup "scalable-chaselev" scalableCL, 
                            bgroup "scalable-unbox" scalableUB,
                            bgroup "hybrid"   hybrid,
                            bgroup "basic"    basicBenchmarks
                          ])

  withArgs args' $ defaultMain list

  where -- _sizes = [10^e | e <- [0..4]]
        parSizes = [ 10000, 100000, 500000 ]
        hotkeySize = 100000

-- Inclusive/Inclusive
for_ :: Monad m => Int64 -> Int64 -> (Int64 -> m a) -> m ()
-- for_ start end _ | start > end = error "start greater than end"
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

{-# INLINE forkJoin #-}
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
