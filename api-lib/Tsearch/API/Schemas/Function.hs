{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Tsearch.API.Schemas.Function where

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



import Tsearch.API.Schemas.FunctionOrType



data Function = Function
    {
        name :: Data.Text.Text,
        type' :: FunctionOrType
    }
    deriving (Show)

instance Data.Aeson.ToJSON Function where
    toJSON Function {..} = Data.Aeson.object
        [
            "name" Data.Aeson..= name,
            "type" Data.Aeson..= type'
        ]

    toEncoding Function {..} = Data.Aeson.Encoding.pairs
        ( Data.Aeson.Encoding.pair "name" (Data.Aeson.toEncoding name) <>
          Data.Aeson.Encoding.pair "type" (Data.Aeson.toEncoding type')
        )

instance Data.Aeson.FromJSON Function where
    parseJSON = Data.Aeson.withObject "Function" $ \o ->
        Function
            <$> o Data.Aeson..: "name"
            <*> o Data.Aeson..: "type"