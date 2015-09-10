module EntryVal
       (EntryRef)
       where

import Data.IORef
import Data.Atomics

newtype EntryRef t = EntryRef (IORef (EntryVal t))
data EntryVal t = Val t | Copied t

