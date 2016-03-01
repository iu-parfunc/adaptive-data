{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.PureMap

main :: IO ()
main = do m :: PureMap Int Int <- newMap
          ins 1 1 m
          get 1 m >>= print
