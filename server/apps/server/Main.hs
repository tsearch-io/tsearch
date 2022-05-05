{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import Tsearch.API.Api (Api(Api))
import qualified Tsearch.API.Api as Api
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import Tsearch.API.Response.Search (SearchResponse(..))
import Data.Text (Text)
import qualified Network.HTTP.Types.Status as Wai

searchHandler :: Text -> IO SearchResponse
searchHandler _ = pure $ SearchResponse200 []

api :: Api IO
api = Api { Api.search = searchHandler }

run :: Wai.Request -> IO a -> IO a
run _ ma = ma

notFound :: Wai.Application
notFound _req respond = respond $ Wai.responseLBS Wai.status404 [] mempty

server :: Wai.Application
server = Api.application run api notFound

main :: IO ()
main = do
  let port = 8080
  putStrLn $ "Running on port " <> show port
  Warp.run port server
