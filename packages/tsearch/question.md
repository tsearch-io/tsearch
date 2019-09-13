I have this simple Servant API/server defintion

```hs
serverMain :: Int -> IO ()
serverMain port = Warp.run port app

app :: Application
app = serve (Proxy :: Proxy TsearchAPI) tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchServer :: Server TsearchAPI
tsearchServer = helloHandler :<|> queryHandler

type HelloHandler = Get '[ PlainText] Text

helloHandler :: Handler Text
helloHandler = undefined

type SearchHandler = 
  "search" :>
  QueryParam "q" String :>
  Get '[ JSON] FunctionRecord

queryHandler :: Maybe String -> Handler FunctionRecord
queryHandler _ = undefined
```

It works and type checks as you would expect.

Since I want to add JSON errors I started using `servant-checked-exceptions`:

```diff
 serverMain :: Int -> IO ()
 serverMain port = Warp.run port app
 
 app :: Application
 app = serve (Proxy :: Proxy TsearchAPI) tsearchServer
 
 type TsearchAPI = HelloHandler :<|> SearchHandler
 
 tsearchServer :: Server TsearchAPI
 tsearchServer = helloHandler :<|> queryHandler
 
-type HelloHandler = Get '[ PlainText] Text
+type HelloHandler = E.NoThrow :> Get '[ PlainText] Text
 
-helloHandler :: Handler Text
+helloHandler :: Handler (E.Envelope '[] Text)
 helloHandler = undefined
 
 type SearchHandler = 
   "search" :>
   QueryParam "q" String :>
+  E.Throws ResponseError :>
   Get '[ JSON] FunctionRecord
 
-queryHandler :: Maybe String -> Handler FunctionRecord
+queryHandler :: Maybe String -> Handler (E.Envelope '[ ResponseError] FunctionRecord)
 queryHandler _ = undefined
```

But now I get an error

```
/home/gillchristian/dev/ts-earch/packages/tsearch/src/Tsearch.hs:60:17: error:
    • Couldn't match type ‘E.Envelope '[ResponseError] FunctionRecord’
                     with ‘FunctionRecord’
      Expected type: Server TsearchAPI
        Actual type: Handler (E.Envelope '[] Text)
                     :<|> (Maybe String
                           -> Handler (E.Envelope '[ResponseError] FunctionRecord))
    • In the expression: helloHandler :<|> queryHandler
      In an equation for ‘tsearchServer’:
          tsearchServer = helloHandler :<|> queryHandler
   |
60 | tsearchServer = helloHandler :<|> queryHandler
```

And I cannot find what's wrong.

I've followed the `servant-checked-exceptions` docs/example and can't find what
I'm doing wrong :confused:
