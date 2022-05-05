{-# LANGUAGE OverloadedStrings #-}

module Tsearch.API.Request
  ( pathVariable,
    requiredQueryParameter,
    optionalQueryParameter,
    requiredHeader,
    optionalHeader,
    parseRequestBody,
    jsonBodyParser,
    formBodyParser,
  )
where

import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import Data.Attoparsec.ByteString (eitherResult, parseWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (HeaderName, hContentType)
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafeInterleaveIO)
import Web.FormUrlEncoded (FromForm, urlDecodeAsForm)
import Web.HttpApiData
  ( FromHttpApiData,
    parseHeader,
    parseQueryParam,
    parseUrlPiece,
  )

pathVariable ::
  FromHttpApiData a =>
  -- | Path variable value
  Text ->
  (a -> Wai.Application) ->
  Wai.Application
pathVariable value withVariable = \request respond ->
  case parseUrlPiece value of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right x ->
      withVariable x request respond
{-# INLINEABLE pathVariable #-}

requiredQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  (a -> Wai.Application) ->
  Wai.Application
requiredQueryParameter name withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam x request respond
{-# INLINEABLE requiredQueryParameter #-}

optionalQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  -- | Allow empty, e.g. "x="
  Bool ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalQueryParameter name allowEmpty withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      withParam Nothing request respond
    Just Nothing
      | allowEmpty ->
        withParam Nothing request respond
      | otherwise ->
        respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam (Just x) request respond
{-# INLINEABLE optionalQueryParameter #-}

optionalHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      withHeader Nothing request respond
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader (Just x) request respond
{-# INLINEABLE optionalHeader #-}

requiredHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (a -> Wai.Application) ->
  Wai.Application
requiredHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader x request respond
{-# INLINEABLE requiredHeader #-}

data BodyParser a = BodyParser ByteString ((a -> Wai.Application) -> Wai.Application)

jsonBodyParser :: FromJSON a => BodyParser a
jsonBodyParser = BodyParser "application/json" parseRequestBodyJSON
{-# INLINE jsonBodyParser #-}

formBodyParser :: FromForm a => BodyParser a
formBodyParser = BodyParser "application/xxx-form-urlencoded" parseRequestBodyForm
{-# INLINE formBodyParser #-}

parseRequestBody :: [BodyParser a] -> (a -> Wai.Application) -> Wai.Application
parseRequestBody parsers withBody = \request respond -> do
  let contentType =
        fromMaybe
          "text/html"
          (List.lookup hContentType (Wai.requestHeaders request))

      bodyParser =
        List.find
          (\(BodyParser expectedContentType _) -> expectedContentType == contentType)
          parsers

  case bodyParser of
    Just (BodyParser _ parseBody) ->
      parseBody withBody request respond
    Nothing ->
      respond (Wai.responseBuilder (toEnum 415) [] mempty)
{-# INLINE parseRequestBody #-}

parseRequestBodyJSON :: FromJSON a => (a -> Wai.Application) -> Wai.Application
parseRequestBodyJSON withBody = \request respond -> do
  result <- parseWith (Wai.getRequestBodyChunk request) Data.Aeson.Parser.json' mempty
  case eitherResult result of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right value ->
      case Data.Aeson.Types.parseEither Data.Aeson.parseJSON value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right body ->
          withBody body request respond
{-# INLINEABLE parseRequestBodyJSON #-}

parseRequestBodyForm :: FromForm a => (a -> Wai.Application) -> Wai.Application
parseRequestBodyForm withBody = \request respond -> do
  -- Reads the body using lazy IO. Not great but it gets us
  -- going and is pretty local.
  let getBodyBytes :: IO [ByteString]
      getBodyBytes = do
        chunk <- Wai.getRequestBodyChunk request
        case chunk of
          "" -> pure []
          _ -> do
            rest <- unsafeInterleaveIO getBodyBytes
            pure (chunk : rest)

  bytes <- getBodyBytes
  case urlDecodeAsForm (LBS.fromChunks bytes) of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right form ->
      withBody form request respond
{-# INLINEABLE parseRequestBodyForm #-}
