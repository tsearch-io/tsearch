{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Response.Search where

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

import Tsearch.API.Schemas.SearchError
import Tsearch.API.Schemas.SearchResult

import Tsearch.API.Response



data SearchResponse
    = SearchResponse200 [ SearchResult ]
    | SearchResponse404
    | SearchResponse422 SearchError
    deriving (Show)

instance Control.Exception.Exception SearchResponse

instance ToResponse SearchResponse where
    toResponse (SearchResponse200 x) =
        Network.Wai.responseBuilder (toEnum 200) ([(Network.HTTP.Types.hContentType, "application/json")]) (Data.Aeson.fromEncoding (Data.Aeson.toEncoding x))
    toResponse (SearchResponse404) =
        Network.Wai.responseBuilder (toEnum 404) ([]) mempty
    toResponse (SearchResponse422 x) =
        Network.Wai.responseBuilder (toEnum 422) ([(Network.HTTP.Types.hContentType, "application/json")]) (Data.Aeson.fromEncoding (Data.Aeson.toEncoding x))