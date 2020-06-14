{-# OPTIONS_GHC -Wall -Werror #-}
import Control.Applicative ((<|>), many)
import Options.Applicative (Parser, ParserInfo)
import System.FilePath ((</>))
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Options.Applicative as Opts
import qualified System.Environment
import qualified System.Exit
import qualified System.IO
import qualified Tsearch.Query as Tsearch
import qualified Tsearch.Search as Tsearch
import qualified Text.Parsec as Parsec
import Data.Foldable (traverse_)

data Options = Options
  { maybeIndex :: Maybe FilePath
  , queries    :: [String]
  }

sample :: Parser Options
sample = Options
      <$> Opts.optional (Opts.strOption
          ( Opts.long "index"
         <> Opts.metavar "INDEX"
         <> Opts.help "Path to index file." ))
      <*> many (Opts.strArgument (Opts.metavar "QUERY"))

getIndex :: Maybe FilePath -> [(String, String)] -> Maybe FilePath
getIndex def env = def <|> maybeDataDir
  where
  maybeDataDir :: Maybe FilePath
  maybeDataDir = (</> "tsearch" </> "index.json") <$> xdgDataDir
  xdgDataDir :: Maybe FilePath
  xdgDataDir = xdg <|> home
  xdg = lookup "XDG_DATA_HOME" env
  home = (</> ".local" </> "share") <$> lookup "HOME" env

options :: ParserInfo Options
options = Opts.info (Opts.helper <*> sample)
      ( Opts.fullDesc
     <> Opts.progDesc ""
     <> Opts.header "tsearch - type-directed search for TypeScript" )

main :: IO ()
main = do
  Options{maybeIndex, queries} <- Opts.execParser options
  env <- System.Environment.getEnvironment
  case getIndex maybeIndex env of
    Nothing    -> do
      System.IO.hPutStrLn System.IO.stderr "Couldn't find index"
      System.Exit.exitFailure
    Just index -> do
      Data.Aeson.eitherDecodeFileStrict' @[Tsearch.FunctionRecord] index >>= \case
        Left e  -> System.IO.hPutStrLn System.IO.stderr e >> System.Exit.exitFailure
        Right v -> traverse_ go queries
          where
          go q = case Parsec.parse Tsearch.query "" q of
            Left e -> System.IO.hPutStrLn System.IO.stderr (show e) >> System.Exit.exitFailure
            Right res -> ByteString.putStrLn $ Data.Aeson.encode $ Tsearch.search res v
