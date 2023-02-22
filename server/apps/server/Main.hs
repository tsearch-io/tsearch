{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Types.Status as Wai
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Tsearch
import Tsearch.API.Api (Api (Api))
import qualified Tsearch.API.Api as Api
import Tsearch.API.Response.Search (SearchResponse (..))
import qualified Tsearch.API.Schemas.Function as Function
import qualified Tsearch.API.Schemas.FunctionOrType as FunctionOrType
import qualified Tsearch.API.Schemas.SearchError as SearchError
import qualified Tsearch.API.Schemas.SearchResult as SearchResult

corsMiddleware :: Wai.Middleware
corsMiddleware =
  Cors.cors $ Just . allowSameOriginPolicy . List.lookup "origin" . Wai.requestHeaders

defaultPolicy :: Cors.CorsResourcePolicy
defaultPolicy =
  Cors.CorsResourcePolicy
    { Cors.corsOrigins = Nothing,
      Cors.corsMethods = ["OPTIONS", "HEAD", "GET", "PUT", "POST", "DELETE"],
      Cors.corsRequestHeaders =
        Cors.simpleHeaders ++ ["Authorization", "Content-Type"],
      Cors.corsExposedHeaders = Just Cors.simpleResponseHeaders,
      Cors.corsMaxAge = Just $ 60 * 60, -- hour in seconds
      Cors.corsVaryOrigin = True,
      Cors.corsRequireOrigin = True,
      Cors.corsIgnoreFailures = False
    }

-- | CORS policy to allow same origin, otherwise fallback to `*`.
-- For requests sending credentials, CORS doesn't allow
-- `Access-Control-Allow-Origin` to be set to `*`, it requires an explicit origin.
allowSameOriginPolicy :: Maybe Cors.Origin -> Cors.CorsResourcePolicy
allowSameOriginPolicy origin =
  defaultPolicy
    { Cors.corsOrigins = fmap (\o -> ([o], True)) origin,
      Cors.corsRequireOrigin = False
    }

searchHandler :: Text -> IO SearchResponse
searchHandler query = do
  Text.putStrLn query

  pure $ case map (Text.unpack . Text.strip) $ Text.splitOn "=>" $ Text.strip query of
    [] -> SearchResponse422 $ SearchError.SearchErrorMissingQuery
    [_] -> SearchResponse422 $ SearchError.SearchErrorInavlidQuery
    signature ->
      SearchResponse200 $
        search $
          Tsearch.TsSignature (init signature) (last signature)
  where
    mkFunc :: Tsearch.TsFunc -> SearchResult.SearchResult
    mkFunc func = SearchResult.SearchResultFunction $ Function.Function (Text.pack func.name) FunctionOrType.FunctionOrTypeFunc

    search q =
      map mkFunc $
        catMaybes $
          map (`Map.lookup` Tsearch.index) $
            Tsearch.searchBySignature Tsearch.trie $
              q

api :: Api IO
api = Api {Api.search = searchHandler}

run :: Wai.Request -> IO a -> IO a
run _ ma = ma

notFound :: Wai.Application
notFound _req respond = respond $ Wai.responseLBS Wai.status404 [] mempty

server :: Wai.Application
server = corsMiddleware $ Api.application run api notFound

main :: IO ()
main = do
  let port = 8000
  putStrLn $ "Running on port " <> show port
  Warp.run port server
