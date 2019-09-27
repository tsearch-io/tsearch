module Main where

import           Control.Monad                        (when)
import qualified Data.Aeson                           as Json
import           Data.Either                          (fromRight)
import           Data.Maybe                           (fromMaybe)
import qualified System                               as Sys
import qualified System.Environment                   as Env
import qualified System.Exit                          as Exit
import qualified Text.ParserCombinators.Parsec.Number as N
import qualified Toolbelt
import qualified Tsearch

dieOnLeft :: Either String a -> IO a
dieOnLeft = either Exit.die pure

flattenModules :: [Tsearch.Module] -> [Tsearch.FunctionRecord]
flattenModules ms = Tsearch.mFns =<< ms

parsePort :: Maybe String -> Int
parsePort = fromRight 8080 . Toolbelt.regularParse N.int . fromMaybe "8080"

main :: IO ()
main = do
  modulesPath <- Sys.argOr 0 "./modules.json"
  fns <-
    flattenModules <$> (dieOnLeft =<< Json.eitherDecodeFileStrict' modulesPath)
  when (null fns) $ Exit.die "Modules are empty"
  port <- parsePort <$> Env.lookupEnv "PORT"
  putStrLn $ "{ \"init\": \"Server starting in port " ++ show port ++ "\" }" -- JSON \o/
  Tsearch.serverMain fns port
