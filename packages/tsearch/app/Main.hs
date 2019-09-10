module Main where

import qualified Tsearch.Server as Tsearch

main :: IO ()
main = do
  putStrLn "Server starting in port 8080"
  Tsearch.serverMain 8080
