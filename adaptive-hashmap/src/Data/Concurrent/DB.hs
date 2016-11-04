module Data.Concurrent.DB where
import Data.ByteString.Lazy

class DB a where
  insert :: Int -> ByteString -> a -> IO ()
  delete :: Int -> a -> IO ()
  lookup :: Int -> a -> IO (Maybe ByteString)
  transition :: a -> IO ()
  output :: a -> IO ()
