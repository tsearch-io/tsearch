{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tsearch
  ( serverMain
  , tsearchServer
  ) where

import qualified Data.Aeson                                      as Json
import qualified Data.ByteString.Lazy.Char8                      as Bs
import           Data.Functor                                    (($>))
import qualified Data.List                                       as List
import           Data.Text                                       (Text)
import           GHC.Generics
import           Network.HTTP.Types                              (status400)
import qualified Network.Wai.Handler.Warp                        as Warp
import           Servant
import qualified Text.Parsec.Char                                as C
import qualified Text.Parsec.Combinator                          as Comb
import           Text.Parsec.Prim                                ((<?>), (<|>))
import qualified Text.Parsec.Prim                                as Parsec
import qualified Text.Parsec.Prim                                as P
import           Text.Parsec.String                              (Parser)
import qualified Text.ParserCombinators.Parsec.Number            as N

serverMain :: Int -> IO ()
serverMain port = Warp.run port app

app :: Application
app = serve (Proxy :: Proxy TsearchAPI) tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchServer :: Server TsearchAPI
tsearchServer = helloHandler :<|> queryHandler

type HelloHandler = Get '[ PlainText] Text

helloHandler :: Handler Text
helloHandler = pure "Hello world"

type SearchHandler = 
  "search" :>
  QueryParam "q" String :>
  Get '[ JSON] [FunctionRecord]

queryHandler ::
     Maybe String -> Handler [FunctionRecord]
queryHandler (Just q) =
  case Parsec.parse signatureP "" q of
    Right r ->
      pure [FunctionRecord Nothing Nothing Nothing "module" r]
    Left e -> throwError $ err404 { errBody = Bs.pack $ show e }
queryHandler Nothing = throwError $ err404 { errBody = "missing query" } 

-- Types ---
data FunctionRecord = FunctionRecord
  { name      :: Maybe String
  , docs      :: Maybe String
  , text      :: Maybe String
  -- , location  :: Location
  , module'   :: String
  , signature :: Signature
  } deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

data PrimitiveType
  = PNumber
  | PString
  | PBool
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON PrimitiveType where
  toJSON PNumber = "number"
  toJSON PString = "string"
  toJSON PBool   = "boolean"

data TypeParameter
  = Polymorphic String
  | Constrained String
                Type
  | WithDefault String
                Type
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON TypeParameter where
  toJSON (Polymorphic name) =
    Json.object [("__tag", "Polymorphic"), ("name", Json.toJSON name)]
  toJSON (Constrained name type') =
    Json.object
      [ ("__tag", "Constrained")
      , ("name", Json.toJSON name)
      , ("constraint", Json.toJSON type')
      ]
  toJSON (WithDefault name type') =
    Json.object
      [ ("__tag", "WithDefault")
      , ("name", Json.toJSON name)
      , ("default", Json.toJSON type')
      ]

data Parameter =
  Parameter String
            Type
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON Parameter where
  toJSON (Parameter name type') =
    Json.object [("name", Json.toJSON name), ("type", Json.toJSON type')]

data Signature =
  Signature [TypeParameter]
            [Parameter]
            Type
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON Signature where
  toJSON (Signature tps ps rt) =
    Json.object
      [ ("typeParameters", Json.toJSON tps)
      , ("parameters", Json.toJSON ps)
      , ("returnType", Json.toJSON rt)
      ]

data Type
  = Any
  | Unkown
  | Undefined
  | LiteralString String
  | LiteralNumber Float
  | LiteralBoolean Bool
  | Primitive String
              PrimitiveType
  | ArrayT String
           Type
  | Union String
          [Type]
  | Intersection String
                 [Type]
  | Tuple String
          [Type]
  | Function String
             [Signature]
  | HigherOrder String
                [Type]
  | Other String
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON Type where
  toJSON Any = Json.object [("__tag", "Any")]
  toJSON Unkown = Json.object [("__tag", "unkown")]
  toJSON Undefined = Json.object [("__tag", "undefined")]
  toJSON (LiteralString str) =
    Json.object [("__tag", "LiteralPrimitive"), ("text", Json.toJSON str)]
  toJSON (LiteralNumber num) =
    Json.object [("__tag", "LiteralPrimitive"), ("text", Json.toJSON num)]
  toJSON (LiteralBoolean bool) =
    Json.object [("__tag", "LiteralPrimitive"), ("text", Json.toJSON bool)]
  toJSON (Primitive text type') =
    Json.object
      [ ("__tag", "Primitive")
      , ("text", Json.toJSON text)
      , ("typeName", Json.toJSON type')
      ]
  toJSON (ArrayT text type') =
    Json.object
      [ ("__tag", "Array")
      , ("text", Json.toJSON text)
      , ("elementsType", Json.toJSON type')
      ]
  toJSON (Union text type') =
    Json.object
      [ ("__tag", "Union")
      , ("text", Json.toJSON text)
      , ("types", Json.toJSON type')
      ]
  toJSON (Intersection text type') =
    Json.object
      [ ("__tag", "Intersection")
      , ("text", Json.toJSON text)
      , ("types", Json.toJSON type')
      ]
  toJSON (Tuple text type') =
    Json.object
      [ ("__tag", "Tuple")
      , ("text", Json.toJSON text)
      , ("types", Json.toJSON type')
      ]
  toJSON (Function text signatures) =
    Json.object
      [ ("__tag", "Function")
      , ("text", Json.toJSON text)
      , ("signatures", Json.toJSON signatures)
      ]
  toJSON (HigherOrder text type') =
    Json.object
      [ ("__tag", "Function")
      , ("text", Json.toJSON text)
      , ("arguments", Json.toJSON type')
      ]
  toJSON (Other text) =
    Json.object [("__tag", "Other"), ("text", Json.toJSON text)]

primitiveTypeToString :: PrimitiveType -> String
primitiveTypeToString PString = "string"
primitiveTypeToString PBool   = "boolean"
primitiveTypeToString PNumber = "number"

stringOfParam :: Parameter -> String
stringOfParam (Parameter n t) = n ++ ": " ++ stringOfType t

params :: [Parameter] -> String
params = List.intercalate ", " . map stringOfParam

typeParams :: [TypeParameter] -> String
typeParams = List.intercalate ", " . map stringOfTypeParam

fnToString :: [Parameter] -> Type -> String
fnToString ps rt = "(" ++ params ps ++ ") => " ++ stringOfType rt

signatureToString :: Signature -> String
signatureToString (Signature [] ps rt) = fnToString ps rt
signatureToString (Signature tps ps rt) =
  "<" ++ typeParams tps ++ ">" ++ fnToString ps rt

stringOfTypeParam :: TypeParameter -> String
stringOfTypeParam (Polymorphic s)   = s
stringOfTypeParam (Constrained s t) = s ++ " extends " ++ stringOfType t
stringOfTypeParam (WithDefault s d) = s ++ " = " ++ stringOfType d

stringOfType :: Type -> String
stringOfType Any = "any"
stringOfType Unkown = "unkown"
stringOfType Undefined = "undefine"
stringOfType (LiteralString name) = "\"" ++ name ++ "\""
stringOfType (LiteralNumber num) = show num
stringOfType (LiteralBoolean True) = "true"
stringOfType (LiteralBoolean False) = "false"
stringOfType (Primitive _ t) = primitiveTypeToString t
stringOfType (ArrayT _ t) = stringOfType t ++ "[]"
stringOfType (Union _ ts) = List.intercalate " | " $ map stringOfType ts
stringOfType (Intersection _ ts) = List.intercalate " & " $ map stringOfType ts
stringOfType (Tuple _ ts) =
  "[" ++ List.intercalate " & " (map stringOfType ts) ++ "]"
stringOfType (Function _ []) = "" -- /shrug
stringOfType (Function _ (s:_)) = signatureToString s
stringOfType (HigherOrder t as) =
  t ++ "<" ++ (List.intercalate ", " . map stringOfType $ as) ++ ">"
stringOfType (Other s) = s

-- QUERY ---
signatureP :: Parser Signature
signatureP = do
  C.spaces
  params <- Comb.sepBy (C.spaces *> type' <* C.spaces) (C.char ',')
  C.string "=>"
  C.spaces
  -- TODO: parse params ???
  Signature [] (Parameter "t" <$> params) <$> type'

type' :: Parser Type
type' =
  P.try typePrimitive <|>
  P.try typeAny <|>
  P.try typeUnkown <|>
  P.try typeUndefined <|>
  P.try typeBoolLit <|>
  P.try typeStringLit <|>
  typeNumLit

checkEnds :: Parser ()
checkEnds = Comb.notFollowedBy C.alphaNum

isArray :: Parser () -- TODO: lookAhead ?
isArray = Comb.notFollowedBy $ C.string "[]"

typeAny :: Parser Type
typeAny =
  P.try $ do
    C.string "any"
    checkEnds
    pure Any

typeUnkown :: Parser Type
typeUnkown =
  P.try $ do
    C.string "unkown"
    checkEnds
    pure Unkown

typeUndefined :: Parser Type
typeUndefined =
  P.try $ do
    C.string "undefined"
    checkEnds
    pure Undefined

typeStringLit :: Parser Type
typeStringLit =
  LiteralString <$> (P.try singleQuoteString <|> doubleQuoteString)

singleQuoteString :: Parser String
singleQuoteString = between (C.char '"') (Comb.many1 $ C.noneOf "\"\n")

doubleQuoteString :: Parser String
doubleQuoteString = between (C.char '\'') (Comb.many1 $ C.noneOf "'\n")

typeBoolLit :: Parser Type
typeBoolLit =
  LiteralBoolean
    <$> (P.try (C.string "true" $> True) <|> (C.string "false" $> False))

typeNumLit :: Parser Type
typeNumLit = LiteralNumber <$> N.floating2 False

typePrimitive :: Parser Type
typePrimitive = Primitive "" <$> primitives

primitives :: Parser PrimitiveType
primitives =
  P.try (C.string "string" $> PString) <|>
  P.try (C.string "number" $> PNumber) <|>
  (C.string "boolean" $> PBool)

between :: Parser a -> Parser b -> Parser b
between limit = Comb.between limit limit
