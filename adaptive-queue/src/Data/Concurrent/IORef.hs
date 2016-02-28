{-# LANGUAGE BangPatterns #-}

module Data.Concurrent.IORef (
    IORef,
    IOVal,
    newIORef,
    readIORef,
    writeIORef,
    casIORef,
    readForCAS,
    peekTicket,
    freezeIORef,
    CASIORefException(CASIORefException),
    TransitionException(TransitionException),
    ) where

import           Control.Exception
import           Control.Monad
import qualified Data.Atomics      as A
import qualified Data.IORef        as IR
import           Data.Typeable

newtype IORef t = IORef (IR.IORef (IOVal t))
  deriving (Eq, Typeable)

data IOVal t = Val !t
             | Frozen !t
  deriving (Show, Typeable)

data CASIORefException = CASIORefException
  deriving (Show, Typeable)

instance Exception CASIORefException

data TransitionException = TransitionException
  deriving (Show, Typeable)

instance Exception TransitionException

{-# INLINABLE newIORef #-}
newIORef :: a -> IO (IORef a)
newIORef !a = do
  !ref <- IR.newIORef $ Val a
  return $ IORef ref

{-# INLINABLE readIORef #-}
readIORef :: IORef a -> IO a
readIORef (IORef !ref) = do
  !v <- IR.readIORef ref
  case v of
    Val t    -> return t
    Frozen t -> return t

{-# INLINABLE writeIORef #-}
writeIORef :: IORef a -> a -> IO ()
writeIORef io !a = readForCAS io >>= loop
  where
    loop tik = do
      (success, tik') <- casIORef io tik a
      unless success $ loop tik'

{-# INLINABLE readForCAS #-}
readForCAS :: IORef a -> IO (A.Ticket (IOVal a))
readForCAS (IORef !ref) = A.readForCAS ref

{-# INLINABLE peekTicket #-}
peekTicket :: A.Ticket (IOVal a) -> a
peekTicket !tik =
  case A.peekTicket tik of
    Val t    -> t
    Frozen t -> t

{-# INLINABLE casIORef #-}
casIORef :: IORef a -> A.Ticket (IOVal a) -> a -> IO (Bool, A.Ticket (IOVal a))
casIORef (IORef !ref) !tik !a =
  case A.peekTicket tik of
    Frozen _ -> throwIO CASIORefException
    Val _    -> A.casIORef ref tik $ Val a

{-# INLINABLE freezeIORef #-}
freezeIORef :: IORef a -> A.Ticket (IOVal a) -> IO (Bool, A.Ticket (IOVal a))
freezeIORef (IORef !ref) !tik =
  case A.peekTicket tik of
    Frozen _ -> return (True, tik)
    Val a    -> A.casIORef ref tik $ Frozen a
