{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tsearch.Server
  ( serverMain
  ) where

import qualified Data.ByteString.Lazy.Char8 as Bs
import           Data.Text                  (Text)
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
import qualified Text.Parsec.Prim           as Parsec

import qualified Tsearch.Query              as TsQ
import qualified Tsearch.Types              as TsT

type TsearchAPI = Get '[ PlainText] Text 
             :<|> "search" :> QueryParam "q" String
                           :> Get '[ JSON] TsT.FunctionRecord

app :: Application
app = serve tsearchApi server

serverMain :: Int -> IO ()
serverMain port = Warp.run port app

tsearchApi :: Proxy TsearchAPI
tsearchApi = Proxy

server :: Server TsearchAPI
server = hello :<|> query

hello :: Handler Text
hello = pure "Hello world"

query :: Maybe String -> Handler TsT.FunctionRecord
query (Just q) =
  case Parsec.parse TsQ.signature "" q of
    Right r -> pure $ TsT.FunctionRecord Nothing Nothing Nothing "module" r
    Left e  -> throwError $ err400 {errBody = Bs.pack $ show e}
query Nothing = throwError $ err400 {errBody = "missing query"}
