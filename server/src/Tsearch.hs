{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import Data.Default.Class (Default (def))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types (status400)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import qualified Network.Wreq as Wreq
import Servant ((:<|>) (..), (:>))
import qualified Servant
import qualified Servant.Checked.Exceptions as E
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import qualified Text.Parsec as P
import Toolbelt
import Tsearch.Query
import Tsearch.Search

-- SERVER ---
serverMain :: [FunctionRecord] -> Int -> IO ()
serverMain fns port = do
  let settings =
        def {Log.outputFormat = Log.CustomOutputFormatWithDetails formatAsJSON}
  logger <- Log.mkRequestLogger settings
  Warp.run port $ (simpleCors . logger) $ app fns

app :: [FunctionRecord] -> Servant.Application
app = Servant.serve tsearchAPI . tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchAPI :: Servant.Proxy TsearchAPI
tsearchAPI = Servant.Proxy

tsearchServer :: [FunctionRecord] -> Servant.Server TsearchAPI
tsearchServer fns = helloHandler :<|> searchHandler fns

type HelloHandler = E.NoThrow :> Servant.Get '[Servant.JSON] Text

helloHandler :: Servant.Handler (E.Envelope '[] Text)
helloHandler = E.pureSuccEnvelope "Hello world"

type SearchHandler =
  "search"
    :> Servant.QueryParam "query" String
    :> E.Throws ResponseError
    :> Servant.Get '[Servant.JSON] [FunctionRecord]

type SearchHandler' = Servant.Handler (E.Envelope '[ResponseError] [FunctionRecord])

searchHandler :: [FunctionRecord] -> Maybe String -> SearchHandler'
searchHandler fns (Just q) =
  case P.parse query "" q of
    Right query' -> do
      liftIO $ putStrLn $ showQuery query'
      let result = search query' fns
      when False $ void . liftIO . forkIO $ publishToAnalytics query' result
      E.pureSuccEnvelope result
    Left e -> E.pureErrEnvelope $ InvalidQuery $ show e
searchHandler _ Nothing = E.pureErrEnvelope MissingQuery

publishToAnalytics :: Query -> [FunctionRecord] -> IO ()
publishToAnalytics q fns = do
  let payload = AnalyticsPayload (show q) (take 5 fns) (length fns)
  -- TODO: Firebase URL
  r <- Wreq.post "http://localhost:9000" (Json.toJSON payload)
  let status = r ^. Wreq.responseStatus . Wreq.statusCode
  putStrLn "Published Analytics"
  putStrLn $ "Status: " ++ show status
  putStrLn $ "Query: " ++ show q
  putStrLn $ show (length fns) ++ " results"

data AnalyticsPayload = AnalyticsPayload
  { apQuery :: String,
    apResult :: [FunctionRecord],
    apCount :: Int
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON AnalyticsPayload where
  toJSON = Json.genericToJSON $ dropLabelPrefix 2

data ResponseError
  = MissingQuery
  | InvalidQuery String
  deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

instance ErrStatus ResponseError where
  toErrStatus _ = status400
