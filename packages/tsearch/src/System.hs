module System
  ( nthArg
  , argOr
  , putStderr
  ) where

import           Control.Applicative (liftA2)
import           Data.Maybe          (fromMaybe)
import qualified System.Environment  as Env
import qualified System.IO           as Sys

nthArg :: Int -> IO (Maybe String)
nthArg = liftA2 nth Env.getArgs . pure

argOr :: Int -> String -> IO String
argOr n def = fromMaybe def <$> nthArg n

putStderr :: String -> IO ()
putStderr = Sys.hPutStrLn Sys.stderr

nth :: [a] -> Int -> Maybe a
nth xs n
  | n >= length xs = Nothing
  | n < 0 = Nothing
  | otherwise = Just (xs !! n)
