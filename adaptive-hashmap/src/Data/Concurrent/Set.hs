module Data.Concurrent.Set where

class DSet a where
  insert :: Int -> a -> IO ()
  delete :: Int -> a -> IO ()
  member :: Int -> a -> IO (Bool)
  transition :: a -> IO ()
  output :: a -> IO ()
