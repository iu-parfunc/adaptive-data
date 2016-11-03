module Data.Concurrent.DB where
import Data.ByteString

class DB a where
  empty :: IO a
  insert :: Int -> ByteString -> a -> IO ()
  delete :: Int -> a -> IO ()
  lookup :: Int -> a -> IO (Maybe ByteString)
