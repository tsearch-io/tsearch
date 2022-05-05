{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Schemas.SearchError where

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







data SearchError
    = SearchErrorInavlidQuery
    | SearchErrorMissingQuery
    deriving (Eq, Show)

instance Data.Aeson.ToJSON SearchError where
    toJSON x = case x of
        SearchErrorInavlidQuery -> "inavlid-query"
        SearchErrorMissingQuery -> "missing-query"

    toEncoding x = case x of
        SearchErrorInavlidQuery -> Data.Aeson.Encoding.text "inavlid-query"
        SearchErrorMissingQuery -> Data.Aeson.Encoding.text "missing-query"

instance Data.Aeson.FromJSON SearchError where
    parseJSON = Data.Aeson.withText "SearchError" $ \s ->
        case s of
            "inavlid-query" -> pure SearchErrorInavlidQuery
            "missing-query" -> pure SearchErrorMissingQuery
            _ -> fail "invalid enum value"

instance Web.HttpApiData.ToHttpApiData SearchError where
    toQueryParam x = case x of
        SearchErrorInavlidQuery -> "inavlid-query"
        SearchErrorMissingQuery -> "missing-query"

instance Web.HttpApiData.FromHttpApiData SearchError where
    parseUrlPiece x =
        case x of
            "inavlid-query" -> pure SearchErrorInavlidQuery
            "missing-query" -> pure SearchErrorMissingQuery
            _ -> Left "invalid enum value"