module Main where

import qualified Tsearch

main :: IO ()
main = do
  putStrLn "Server starting in port 8080"
  Tsearch.serverMain 8080
