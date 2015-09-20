module EntryRef
       (
         EntryRef,
         EntryVal(Val, Copied),
         newEntryRef
       )
       where

import Data.IORef
-- import Data.Atomics

type EntryRef t = IORef (EntryVal t)
data EntryVal t = Val t | Copied t

newEntryRef :: a -> IO (EntryRef a)
newEntryRef a = newIORef $ Val a
