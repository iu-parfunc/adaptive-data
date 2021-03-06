{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Data.Concurrent.Ctrie
    ( Map

      -- * Construction
    , empty
    , fromList

      -- * Modification
    , insert
    , delete

      -- * Query
    , lookup
    , size

    -- * Freezing and iteration
    , unsafeToList
    , freeze, pollFreeze
    , unsafeTraverse_
    , freezeAndTraverse_
    , freezeFold   
    --, printMap

    , freezeRandBottom
    , freezeRandConvert      
    -- Temporarily exposed:
    , makePerms, Perms, unpackPerms
    ) where

--import Control.Applicative ((<$>))
import           Control.Monad
import           Control.Concurrent
import           Data.Bits
import           Data.Hashable (Hashable)
import qualified Data.Hashable as H
import qualified Data.List     as List
import           Data.Maybe
import           Data.Word
import           Prelude       hiding (lookup)

import           Data.Concurrent.IORef
import qualified Data.IORef as I
import qualified Data.Concurrent.Map.Array as A

-- import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as UM

import System.Random.Shuffle (shuffleM)
import Data.List.Split (chunksOf)
import qualified Data.HashMap.Strict   as HM
-----------------------------------------------------------------------

-- | A map from keys @k@ to values @v@.
newtype Map k v = Map (INode k v)

type INode k v = IORef (MainNode k v)

data MainNode k v = CNode Bitmap (A.Array (Branch k v))
                  | Tomb (SNode k v)
                  | Collision [SNode k v]

data Branch k v = INode (INode k v)
                | SNode (SNode k v)

data SNode k v = S k v
    deriving (Eq, Show)

isTomb :: MainNode k v -> Bool
isTomb (Tomb _) = True
isTomb _        = False

type Bitmap = Word
type Hash   = Word
type Level  = Int

hash :: Hashable a => a -> Hash
hash = fromIntegral . H.hash


-----------------------------------------------------------------------
-- * Construction

-- | /O(1)/. Construct an empty map.
empty :: IO (Map k v)
empty = Map <$> newIORef (CNode 0 A.empty)


-----------------------------------------------------------------------
-- * Modification

-- | /O(log n)/. Associate the given value with the given key.
-- If the key is already present in the map, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> Map k v -> IO ()
insert k v (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            ticket <- readForCAS inode
            case peekTicket ticket of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                        n = popCount bmp
                    if bmp .&. m == 0
                        then do
                            let arr' = A.insert (SNode (S k v)) i n arr
                                cn'  = CNode (bmp .|. m) arr'
                            unlessM (fst <$> casIORef inode ticket cn') go0

                        else case A.index arr i of
                            SNode (S k2 v2)
                                | k == k2 -> do
                                    let arr' = A.update (SNode (S k v)) i n arr
                                        cn'  = CNode bmp arr'
                                    unlessM (fst <$> casIORef inode ticket cn') go0

                                | otherwise -> do
                                    let h2 = hash k2
                                    inode2 <- newINode h k v h2 k2 v2 (nextLevel lev)
                                    let arr' = A.update (INode inode2) i n arr
                                        cn'  = CNode bmp arr'
                                    unlessM (fst <$> casIORef inode ticket cn') go0

                            INode inode2 -> go (nextLevel lev) inode inode2

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision arr -> do
                    let arr' = S k v : filter (\(S k2 _) -> k2 /= k) arr
                        col' = Collision arr'
                    unlessM (fst <$> casIORef inode ticket col') go0

{-# INLINABLE insert #-}

newINode :: Hash -> k -> v -> Hash -> k -> v -> Int -> IO (INode k v)
newINode h1 k1 v1 h2 k2 v2 lev
    | lev >= hashLength = newIORef $ Collision [S k1 v1, S k2 v2]
    | otherwise = do
        let i1 = index h1 lev
            i2 = index h2 lev
            bmp = (unsafeShiftL 1 i1) .|. (unsafeShiftL 1 i2)
        case compare i1 i2 of
            LT -> newIORef $ CNode bmp $ A.pair (SNode (S k1 v1)) (SNode (S k2 v2))
            GT -> newIORef $ CNode bmp $ A.pair (SNode (S k2 v2)) (SNode (S k1 v1))
            EQ -> do inode' <- newINode h1 k1 v1 h2 k2 v2 (nextLevel lev)
                     newIORef $ CNode bmp $ A.singleton (INode inode')


-- | /O(log n)/. Remove the given key and its associated value from the map,
-- if present.
delete :: (Eq k, Hashable k) => k -> Map k v -> IO ()
delete k (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            ticket <- readForCAS inode
            case peekTicket ticket of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                    if bmp .&. m == 0
                        then return ()  -- not found
                        else case A.index arr i of
                            SNode (S k2 _)
                                | k == k2 -> do
                                    let arr' = A.delete i (popCount bmp) arr
                                        cn'  = CNode (bmp `xor` m) arr'
                                        cn'' = contract lev cn'
                                    unlessM (fst <$> casIORef inode ticket cn'') go0
                                    whenM (isTomb <$> readIORef inode) $
                                        cleanParent parent inode h (prevLevel lev)

                                | otherwise -> return ()  -- not found

                            INode inode2 -> go (nextLevel lev) inode inode2

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision arr -> do
                    let arr' = filter (\(S k2 _) -> k2 /= k) $ arr
                        col' | [s] <- arr' = Tomb s
                             | otherwise   = Collision arr'
                    unlessM (fst <$> casIORef inode ticket col') go0

{-# INLINABLE delete #-}

-----------------------------------------------------------------------
-- * Query

-- | /O(log n)/. Return the value associated with the given key, or 'Nothing'.
lookup :: (Eq k, Hashable k) => k -> Map k v -> IO (Maybe v)
lookup k (Map root) = go0
    where
        h = hash k
        go0 = go 0 undefined root
        go lev parent inode = do
            main <- readIORef inode
            case main of
                CNode bmp arr -> do
                    let m = mask h lev
                        i = sparseIndex bmp m
                    if bmp .&. m == 0
                        then return Nothing
                        else case A.index arr i of
                            INode inode2 -> go (nextLevel lev) inode inode2
                            SNode (S k2 v) | k == k2   -> return (Just v)
                                           | otherwise -> return Nothing

                Tomb _ -> clean parent (prevLevel lev) >> go0

                Collision xs -> do
                    case List.find (\(S k2 _) -> k2 == k) xs of
                        Just (S _ v) -> return (Just v)
                        _            -> return Nothing

{-# INLINABLE lookup #-}

-----------------------------------------------------------------------
-- * Internal compression operations

clean :: INode k v -> Level -> IO ()
clean inode lev = do
    ticket <- readForCAS inode
    case peekTicket ticket of
        cn@(CNode _ _) -> do
            cn' <- compress lev cn
            void $ casIORef inode ticket cn'
        _ -> return ()
{-# INLINE clean #-}

cleanParent :: INode k v -> INode k v -> Hash -> Level -> IO ()
cleanParent parent inode h lev = do
    ticket <- readForCAS parent
    case peekTicket ticket of
        cn@(CNode bmp arr) -> do
            let m = mask h lev
                i = sparseIndex bmp m
            unless (bmp .&. m == 0) $
                case A.index arr i of
                    INode inode2 | inode2 == inode ->
                        whenM (isTomb <$> readIORef inode) $ do
                            cn' <- compress lev cn
                            unlessM (fst <$> casIORef parent ticket cn') $
                                cleanParent parent inode h lev
                    _ -> return ()
        _ -> return ()

compress :: Level -> MainNode k v -> IO (MainNode k v)
compress lev (CNode bmp arr) =
    contract lev <$> CNode bmp <$> A.mapM resurrect (popCount bmp) arr
compress _ x = return x
{-# INLINE compress #-}

resurrect :: Branch k v -> IO (Branch k v)
resurrect b@(INode inode) = do
    main <- readIORef inode
    case main of
        Tomb s -> return (SNode s)
        _      -> return b
resurrect b = return b
{-# INLINE resurrect #-}

contract :: Level -> MainNode k v -> MainNode k v
contract lev (CNode bmp arr) | lev > 0
                           , popCount bmp == 1
                           , SNode s <- A.head arr
                           = Tomb s
contract _ x = x
{-# INLINE contract #-}

-----------------------------------------------------------------------
-- * Lists

-- | /O(n * log n)/. Construct a map from a list of key/value pairs.
fromList :: (Eq k, Hashable k) => [(k,v)] -> IO (Map k v)
fromList xs = empty >>= \m -> mapM_ (\(k,v) -> insert k v m) xs >> return m
{-# INLINABLE fromList #-}

-- | /O(n)/. Unsafely convert the map to a list of key/value pairs.
--
-- WARNING: 'unsafeToList' makes no atomicity guarantees. Concurrent
-- changes to the map will lead to inconsistent results.
unsafeToList :: Map k v -> IO [(k,v)]
unsafeToList (Map root) = go root
    where
        go inode = do
            main <- readIORef inode
            case main of
                CNode bmp arr -> A.foldM' go2 [] (popCount bmp) arr
                Tomb (S k v) -> return [(k,v)]
                Collision xs -> return $ map (\(S k v) -> (k,v)) xs

        go2 xs (INode inode) = go inode >>= \ys -> return (ys ++ xs)
        go2 xs (SNode (S k v)) = return $ (k,v) : xs
{-# INLINABLE unsafeToList #-}

-- | An approximation of the CTries size.
--   Concurrent modifications may keep this method from being serialiable.
size :: Map k v -> IO Int
size (Map root) = go root
    where
        go inode = do
            main <- readIORef inode
            case main of
                CNode bmp arr -> A.foldM' go2 0 (popCount bmp) arr
                Tomb (S k v) -> return 1
                Collision xs -> return $! length xs

        go2 xs (INode inode) = do ys <- go inode; return (ys + xs)
        go2 xs (SNode (S k v)) = return $! 1 + xs
{-# INLINABLE size #-}


-- | A procedure that combines freezing and traversal.
freezeAndTraverse_ :: (k -> v -> IO ()) -> Map k v -> IO ()
freezeAndTraverse_ fn (Map root) = go root
  where
    go inode = do
      freezeref inode
      main <- readIORef inode
      case main of
        CNode bmp arr -> A.mapM_ go2 (popCount bmp) arr
        Tomb (S k v) -> fn k v
        Collision xs -> mapM_ (\ (S k v) -> fn k v) xs
    go2 (INode inode) = go inode
    go2 (SNode (S k v)) = fn k v
{-# INLINE freezeAndTraverse_ #-}

freezeref ref = freezeloop ref =<< readForCAS ref

freezeloop ref = spinlock $ freezeIORef ref

-- | Freezes, performs a fold, and returns the size of the structure as well.
-- TODO: Fix so it returns the size.
freezeFold :: (a -> k -> v -> a) -> a -> Map k v -> IO a -- (Int, a)
freezeFold fn zer (Map root) = go zer root
  where
    go !acc inode = do
      freezeref inode
      main <- readIORef inode
      case main of
        CNode bmp arr -> A.foldM' go2 acc (popCount bmp) arr
        Tomb (S k v) -> return $! fn acc k v
        Collision xs -> return $! List.foldl' (\ a (S k v) -> fn a k v) acc xs
    go2 !acc (INode inode)   = go acc inode
    go2 !acc (SNode (S k v)) = return $! fn acc k v
{-# INLINE freezeFold #-}


-- | A non-allocating way to traverse a frozen structure.
unsafeTraverse_ :: (k -> v -> IO ()) -> Map k v -> IO ()
unsafeTraverse_ fn (Map root) = go root
  where
    go inode = do
        main <- readIORef inode
        case main of
            CNode bmp arr -> A.mapM_ go2 (popCount bmp) arr
            Tomb (S k v) -> fn k v
            Collision xs -> mapM_ (\ (S k v) -> fn k v) xs
    go2 (INode inode)   = go inode
    go2 (SNode (S k v)) = fn k v
{-# INLINE unsafeTraverse_ #-}

-- | A blocking O(N) freeze operation that proceeds from root to leaves.
freeze :: Map k v -> IO ()
-- Can't use freezeAndTraverse_ here because of the Collision/mapM_ case.
freeze (Map root) = go root
  where
    go inode = do
      freezeloop inode =<< readForCAS inode
      main <- readIORef inode
      case main of
        CNode bmp arr -> A.mapM_ go2 (popCount bmp) arr
        Tomb (S _ _)  -> return ()
        Collision _   -> return ()
    go2 (INode inode) = go inode
    go2 (SNode (S _ _)) = return ()
{-# INLINABLE freeze #-}

-- | A version of `freeze` which periodically runs a polling action
--  to check if it should keep going.  If the polling action returns
--  `False`, then this procedure returns immediately.
--
--  The final return value is True if this function got to the end
--  without aborting.
pollFreeze :: IO Bool -> Map k v -> IO Bool
pollFreeze poll (Map root) = go root
  where
    go inode = do
      b <- poll
      if b then
        do freezeref inode
           main <- readIORef inode
           case main of
             CNode bmp arr ->
               -- TODO short circuiting mapM_.  Could use maybe monad...
               do A.mapM_ go2 (popCount bmp) arr
                  b <- poll -- Assumed to be monotonic.
                  return b
             Tomb (S _ _)  -> return True
             Collision _   -> return True
       else
        return False
    go2 (INode inode)   = go inode
    go2 (SNode (S _ _)) = return True
{-# INLINE pollFreeze #-}

-----------------------------------------------------------------------

whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = p >>= \t -> if t then s else return ()
{-# INLINE whenM #-}

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = p >>= \t -> if t then return () else s
{-# INLINE unlessM #-}

-----------------------------------------------------------------------

hashLength :: Int
hashLength = finiteBitSize (undefined :: Word)

-- 6 currently [2016.07.06]
bitsPerSubkey :: Int
bitsPerSubkey = floor . logBase (2 :: Float) . fromIntegral $ hashLength

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Hash -> Level -> Int
index h lev = fromIntegral $ (h `unsafeShiftR` lev) .&. subkeyMask
{-# INLINE index #-}

-- when or-ed with a CNode bitmap, determines if the hash is present
-- in the array at the given level of the trie
mask :: Hash -> Level -> Bitmap
mask h lev = 1 `unsafeShiftL` index h lev
{-# INLINE mask #-}

-- position in the CNode array
sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex bmp m = popCount ((m - 1) .&. bmp)
{-# INLINE sparseIndex #-}

nextLevel :: Level -> Level
nextLevel = (+) bitsPerSubkey
{-# INLINE nextLevel #-}

prevLevel :: Level -> Level
prevLevel = subtract bitsPerSubkey
{-# INLINE prevLevel #-}

-----------------------------------------------------------------------

-- TODO
--printMap :: (Show k, Show v) => Map k v -> IO ()
--printMap (Map root) = goI root
--    where
--        goI inode = putStr "(I " >> readIORef inode >>= goM >> putStr ")\n"
--        goM (CNode bmp arr) = do
--            putStr $ "(C " ++ (show bmp) ++ " ["
--            A.mapM_ (\b -> goB b >> putStr ", ") (popCount bmp) arr
--            putStr $ "] )"
--        goM (Tomb (S k v)) = putStr $ "(T " ++ (show k) ++ " " ++ (show v) ++ ")"
--        goM (Collision xs) = putStr $ "(Collision " ++ show xs ++ ")"
--        goB (INode i) = putStr "\n" >> goI i
--        goB (SNode (S k v)) = putStr $ "(" ++ (show k) ++ "," ++ (show v) ++ ")"

-- | Parallel freezing experiment.  This freeze is BOTTOM UP, leaves first.
freezeRandBottom :: Perms -> Map k v -> IO ()
freezeRandBottom perms (Map root) = go root
  where
    go inode = do
      main <- readFRef inode
      -- We need this freezing/frozen state.  Otherwise inode can
      -- change between the recursion and the freezeref call:
      (spinlock $ startFreezeIORef inode) =<< readForCAS inode

      -- ALTERNATIVE: Instead of split freezing/frozen steps, this could be speculative.
      -- That is, if we use casIORef and make sure the value has not changed when we write
      -- (freeze) it, then I think by induction subtrees under us will be ok if we just
      -- check&CAS our level.
                                            
      -- Invariant: by the time we get to a leaf, everything above is FREEZING.
      let dorec x =
            case x of
              CNode bmp arr -> let len = (popCount bmp) in
                               -- assert len > 0 
                               mapPerm_ go2 (permOf perms len) len arr
              Tomb (S _ _)  -> return ()
              Collision _   -> return ()

      case main of
        -- This subtree was already frozen from the bottom, don't recur:
        Frozen _ -> return ()
        -- Policy: For lock-freedom, we need to complete the operation
        -- even if the other thread is stalled:
        Freezing x -> do yield; dorec x
        Val      x -> dorec x

      -- Invariant: by the time our subtrees are processed, everything below is FROZEN.
      freezeref inode
    go2 (INode inode)   = go inode
    go2 (SNode (S _ _)) = return ()


-- | Bottom-up freeze in random order.  Take a HashMap reference as an
-- OUTPUT PARAM.  This function must be reentrant, with multiple calls
-- cooperating to build the output structure.
freezeRandConvert :: forall k v . (Eq k, Hashable k) =>
                     Perms -> Map k v -> I.IORef (HM.HashMap k v) -> IO ()

-- NOTE: A full parallel version is QUADRATIC.  It does an expensive union at each step of
-- the tree.  Thus we instead limit the number of parallel recursions and then bottom out
-- to the sequential version.

freezeRandConvert perms (Map root) outref = go root
  where
    go :: (INode k v) -> IO ()
    go inode = do
      main <- readFRef inode

      (spinlock $ startFreezeIORef inode) =<< readForCAS inode

      -- EMPIRICAL NOTE: With big tries, the first couple levels are 64-wide because the
      -- tree gets heavily populated.  Hence it is ok if we parallelize just ONE level for
      -- now.

      let dorec x =
            case x of
              CNode bmp arr -> do let len = (popCount bmp)
                                  -- putStrLn $ "BRANCHING: "++ show len
                                  -- foldPerm_ (\ !acc x -> fmap (HM.union acc) (go2 x))
                                  --           HM.empty (permOf perms len) len arr

                                  mapPerm_ go2 (permOf perms len) len arr

              Tomb (S k v) -> I.atomicModifyIORef outref (\hm -> (HM.insert k v hm, ()))
-- FINISHME: 
--                   Collision xs -> return $! List.foldl' (\ a (S k v) -> fn a k v) acc xs
                                            
      case main of
        -- case(1): This subtree was already frozen, AND converted:
        Frozen   _ -> return ()
        Freezing x -> do yield; dorec x
        Val      x -> dorec x

      -- INVARIANT: if we return to this point, we have verified that all children are
      -- completed, AND written to the accumulator, by us or by someone else.

      -- Now it's safe to mark it, to prevent further merges of redundant information:
      freezeref inode
      return ()

    go2 (SNode (S k v)) = return $! HM.singleton k v

    -- Don't recur back to the parallel version, instead use the sequential.
    -- go2 (INode inode) = go inode
                          
    -- Process a child node sequentially, and then add it right into the accumulator:
    go2 (INode inode) = do
      -- This should be strict in the accumulator:
      -- hm <- go3 HM.empty inode =<< readForCAS inode
      hm <- go3 HM.empty inode 

      -- This is currently lazy:
      I.atomicModifyIORef outref (\a -> let tr = HM.union hm a
                                        in (tr,tr))
      -- There are several choices to make here about lazy vs strict,
      -- speculative/wasteful vs blackhole-risk:      
             
    fn = (\h k v -> HM.insert k v h)

    -- Copied from freezeFold:
    ----------------------------------------
    -- Switch to full freeze-before-read here.
    -- This version cannot CHECK to see if the banch has already been handled:
    -- go3 !acc inode = do
    --   freezeref inode
    --   main <- readIORef inode
    --   case main of
    --     CNode bmp arr -> A.foldM' go4 acc (popCount bmp) arr
    --     Tomb (S k v) -> return $! fn acc k v
    --     Collision xs -> return $! List.foldl' (\ a (S k v) -> fn a k v) acc xs

    -- Copied from freezeRandBottom and modified:
    ----------------------------------------         
    go3 !acc inode = do
      main <- readFRef inode
      (spinlock $ startFreezeIORef inode) =<< readForCAS inode
      let dorec x =
            case x of
              CNode bmp arr -> A.foldM' go4 acc (popCount bmp) arr
              Tomb (S k v)  -> return $! fn acc k v
              Collision xs  -> return $! List.foldl' (\ a (S k v) -> fn a k v) acc xs
      tree <- case main of
               -- This subtree was ALREADY frozen and added to accum:
               Frozen _ -> return HM.empty -- Possible silly union of empty into the accum!
               -- Policy: For lock-freedom, we need to complete the operation
               -- even if the other thread is stalled:
               Freezing x -> do yield; dorec x
               Val      x -> dorec x
      freezeref inode
      return tree

    ----------------------------------------
    go4 !acc (INode inode)   = go3 acc inode
    go4 !acc (SNode (S k v)) = return $! fn acc k v

       

                          
--------------------------------------------------------------------------------

-- | A collection of random permutations of numbers [0..1], [0..2], ... [0..63].
--   This is a FLATTENED vector of groups of 64 bytes.
--   Each 64 bytes expresses ONE permutation, with mostly wasted bits for the smaller ones.
type Perms = UV.Vector Word8

-- | A vector of exactly 64 bytes.
type Perm = UV.Vector Word8

-- | Extract the permutation of N elements from a full deck of permutations.
permOf :: Perms -> Int -> Perm
permOf p n = UV.slice (64*(n-1)) 64 p

permGet :: Perms -> Int -> Int
permGet p i = fromIntegral $! p UV.! i 

-- | Map in an order designated by the given permutation.
mapPerm_ :: (a -> IO b) -> Perm -> Int -> A.Array a -> IO ()
mapPerm_ f perm n0 arr0 = go n0 arr0 0
    where
        go n arr i
            | i >= n = return ()
            | otherwise = do
                let ix = permGet perm i
                x <- A.indexArrayM arr ix
                _ <- f x
                go n arr (i+1)
{-# INLINE mapPerm_ #-}


foldPerm_ :: (acc -> a -> IO acc) -> acc -> Perm -> Int -> A.Array a -> IO acc
foldPerm_ fn acc0 perm n0 arr0 = go acc0 n0 arr0 0
    where
        go !acc n arr i
            | i >= n = return acc
            | otherwise = do
                let ix = permGet perm i
                x    <- A.indexArrayM arr ix
                acc' <- fn acc x
                go acc' n arr (i+1)
{-# INLINE foldPerm_ #-}

unpackPerms :: Perms -> [[Int]]
unpackPerms v =
  let ll = chunksOf 64 $ UV.toList v in
  [ map fromIntegral $ take n l
  | (n,l) <- zip [1..] ll ]

-- FIXME: this needs to change to 64 to complete this algorithm...
numPerms :: Int
numPerms = 10

makePerms :: IO Perms
makePerms = do
  vs <- sequence $
   [ do -- There's no point in permuting 0 elements:
        ls <- shuffleM [0 .. fromIntegral n]
        return $ UV.fromList $ ls ++ replicate (63 - n) 0 
   | n <- [0..63] ]
  return $! UV.concat vs

