module Control.Concurrent.PureQueue (
    PureQueue,
    newQ,
    nullQ,
    pushL,
    tryPopR,
    ) where

import Control.Monad
import Data.Concurrent.IORef
import Data.Sequence         as S

type PureQueue a = IORef (Seq a)

newQ :: IO (PureQueue a)
newQ = newIORef empty

nullQ :: PureQueue a -> IO Bool
nullQ q = S.null `fmap` readIORef q

pushL :: PureQueue a -> a -> IO ()
pushL q val = loop =<< readForCAS q
  where
    loop tik = do
      let s = peekTicket tik
          s' = val <| s
      (success, tik') <- casIORef q tik s'
      unless success $ loop tik'

tryPopR :: PureQueue a -> IO (Maybe a)
tryPopR q = loop =<< readForCAS q
  where
    loop tik = do
      let s = peekTicket tik
      case viewr s of
        EmptyR -> return Nothing
        s' :> val -> do
          (success, tik') <- casIORef q tik s'
          if success
            then return $ Just val
            else loop tik'
