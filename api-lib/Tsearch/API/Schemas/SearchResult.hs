{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Schemas.SearchResult where

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



import Tsearch.API.Schemas.Function
import Tsearch.API.Schemas.Type



data SearchResult
    = SearchResultFunction Function
    | SearchResultType Type
    deriving (Show)

instance Data.Aeson.ToJSON SearchResult where
    toJSON (SearchResultFunction x) = Data.Aeson.toJSON x
    toJSON (SearchResultType x) = Data.Aeson.toJSON x

    toEncoding (SearchResultFunction x) = Data.Aeson.toEncoding x
    toEncoding (SearchResultType x) = Data.Aeson.toEncoding x

instance Data.Aeson.FromJSON SearchResult where
    parseJSON x =
        (SearchResultFunction <$> (Data.Aeson.Types.withObject "Function" $ \o ->
            do ("func" :: Data.Text.Text) <- o Data.Aeson..: "type"
               Data.Aeson.parseJSON (Data.Aeson.Object o)
        ) x) Control.Applicative.<|>
        (SearchResultType <$> (Data.Aeson.Types.withObject "Type" $ \o ->
            do ("type" :: Data.Text.Text) <- o Data.Aeson..: "type"
               Data.Aeson.parseJSON (Data.Aeson.Object o)
        ) x)