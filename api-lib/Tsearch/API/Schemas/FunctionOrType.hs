{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Schemas.FunctionOrType where

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







data FunctionOrType
    = FunctionOrTypeFunc
    | FunctionOrTypeType
    deriving (Eq, Show)

instance Data.Aeson.ToJSON FunctionOrType where
    toJSON x = case x of
        FunctionOrTypeFunc -> "func"
        FunctionOrTypeType -> "type"

    toEncoding x = case x of
        FunctionOrTypeFunc -> Data.Aeson.Encoding.text "func"
        FunctionOrTypeType -> Data.Aeson.Encoding.text "type"

instance Data.Aeson.FromJSON FunctionOrType where
    parseJSON = Data.Aeson.withText "FunctionOrType" $ \s ->
        case s of
            "func" -> pure FunctionOrTypeFunc
            "type" -> pure FunctionOrTypeType
            _ -> fail "invalid enum value"

instance Web.HttpApiData.ToHttpApiData FunctionOrType where
    toQueryParam x = case x of
        FunctionOrTypeFunc -> "func"
        FunctionOrTypeType -> "type"

instance Web.HttpApiData.FromHttpApiData FunctionOrType where
    parseUrlPiece x =
        case x of
            "func" -> pure FunctionOrTypeFunc
            "type" -> pure FunctionOrTypeType
            _ -> Left "invalid enum value"