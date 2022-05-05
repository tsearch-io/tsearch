{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Api where

import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Control.Monad.IO.Class
import qualified Data.Aeson
import qualified Data.Aeson.Encoding
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.ByteString
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified Data.Text.Encoding
import qualified GHC.Float
import qualified GHC.Int
import qualified GHC.Types
import qualified Network.HTTP.Types
import qualified Network.Wai
import qualified Web.HttpApiData

import Tsearch.API.Request
import Tsearch.API.Response



import Tsearch.API.Schemas.SearchError
import Tsearch.API.Schemas.SearchResult

import Tsearch.API.Response.Search

data Api m = Api {
    -- | Search for TypeScript functions and types
    search ::
        -- @q@
        Data.Text.Text ->
        m SearchResponse
}

application :: (Control.Monad.Catch.MonadCatch m, Control.Monad.IO.Class.MonadIO m) => (forall a . Network.Wai.Request -> m a -> IO a) -> Api m -> Network.Wai.Application -> Network.Wai.Application
application run api notFound request respond =
    case Network.Wai.pathInfo request of
        ["v1", "search"] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    requiredQueryParameter "q" (\__q request respond ->
                        run request (do
                            response <- Control.Monad.Catch.handle pure (search api __q)
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )) request respond
                x ->
                    unsupportedMethod x

        _ ->
            notFound request respond
    where
        unsupportedMethod _ =
            respond (Network.Wai.responseBuilder (toEnum 405) [] mempty)