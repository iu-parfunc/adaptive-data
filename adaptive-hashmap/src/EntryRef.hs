module EntryRef
       (
         EntryRef,
         EntryVal(Val, Copied),
         newEntryRef,
         readEntryRef,
         casEntryRef,
         readEntryRefForCAS,
         peekEntryTicket,
         freezeEntryRef,
         CASEntryRefException
       )
       where

import Data.IORef
import Data.Atomics
import Control.Exception
import Data.Typeable

data EntryRef t = EntryRef (IORef (EntryVal t))
data EntryVal t = Val t | Copied t deriving (Show, Typeable)

data CASEntryRefException = CASEntryRefException
                          deriving (Show, Typeable)
instance Exception CASEntryRefException

{-# INLINABLE newEntryRef #-}
newEntryRef :: a -> IO (EntryRef a)
newEntryRef a = do
  ref <- newIORef $ Val a
  return $ EntryRef ref

{-# INLINABLE readEntryRef #-}
readEntryRef :: EntryRef a -> IO a
readEntryRef (EntryRef ref) = do
  v <- readIORef ref 
  case v of
    Val    t -> return t
    Copied t -> return t

{-# INLINABLE readEntryRefForCAS #-}
readEntryRefForCAS :: EntryRef a -> IO (Ticket (EntryVal a))
readEntryRefForCAS (EntryRef ref) = readForCAS ref 

{-# INLINABLE peekEntryTicket #-}
peekEntryTicket :: Ticket (EntryVal a) -> a
peekEntryTicket tik =
  case peekTicket tik of
  Val t -> t
  Copied t -> t

{-# INLINABLE casEntryRef #-}
casEntryRef :: EntryRef a -> Ticket (EntryVal a) -> a -> IO(Bool, Ticket (EntryVal a))
casEntryRef (EntryRef ref) tik a = do
  case peekTicket tik of
    Copied _ -> throwIO CASEntryRefException
    Val    _ -> casIORef ref tik $ Val a

{-# INLINABLE freezeEntryRef #-}
freezeEntryRef :: EntryRef a -> Ticket (EntryVal a) -> IO(Bool, Ticket (EntryVal a))
freezeEntryRef (EntryRef ref) tik = do
  case peekTicket tik of
    Copied _ -> return (True, tik)
    Val    a -> casIORef ref tik $ Copied a
