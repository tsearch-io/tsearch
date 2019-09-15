{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Tsearch where

import Control.Monad (void)
import qualified Data.Aeson                                      as Json
import           Data.Functor                                    (($>))
import qualified Data.List                                       as List
import           Data.Text                                       (Text)
import           GHC.Generics
import           Network.HTTP.Types                              (status400)
import qualified Network.Wai.Handler.Warp                        as Warp
import           Servant
import qualified Servant.Checked.Exceptions                      as E
import           Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import qualified Text.Parsec.Char                                as C
import qualified Text.Parsec.Combinator                          as Comb
import           Text.Parsec.Prim                                ((<|>))
import qualified Text.Parsec.Prim                                as Parsec
import qualified Text.Parsec.Prim                                as P
import           Text.Parsec.String                              (Parser)
import qualified Text.ParserCombinators.Parsec.Number            as N

-- SERVER ---
serverMain :: Int -> IO ()
serverMain port = Warp.run port app

app :: Application
app = serve tsearchAPI tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchAPI :: Proxy TsearchAPI
tsearchAPI = Proxy

tsearchServer :: Server TsearchAPI
tsearchServer = helloHandler :<|> queryHandler

type HelloHandler = E.NoThrow :> Get '[ JSON] Text

helloHandler :: Handler (E.Envelope '[] Text)
helloHandler = E.pureSuccEnvelope "Hello world"

type SearchHandler = 
  "search" :>
  QueryParam "q" String :>
  E.Throws ResponseError :>
  Get '[ JSON] [FunctionRecord]

queryHandler ::
     Maybe String -> Handler (E.Envelope '[ ResponseError] [FunctionRecord])
queryHandler (Just q) =
  case Parsec.parse signatureP "" q of
    Right r ->
      E.pureSuccEnvelope [FunctionRecord Nothing Nothing Nothing "module" r]
    Left e -> E.pureErrEnvelope $ InvalidQuery $ show e
queryHandler Nothing = E.pureErrEnvelope MissingQuery

-- TODO: this should not be a record, servant-checked-exceptions already wrapps
-- in { data: a } and { err: b }
data ResponseError = 
    MissingQuery 
  | InvalidQuery String deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

-- TODO: change ResponseError to union type with all the cases and then each
-- could have it's own `statusXXX`
instance ErrStatus ResponseError where
  toErrStatus _ = status400

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
  toJSON (Polymorphic n) =
    Json.object [("__tag", "Polymorphic"), ("name", Json.toJSON n)]
  toJSON (Constrained n t) =
    Json.object
      [ ("__tag", "Constrained")
      , ("name", Json.toJSON n)
      , ("constraint", Json.toJSON t)
      ]
  toJSON (WithDefault n t) =
    Json.object
      [ ("__tag", "WithDefault")
      , ("name", Json.toJSON n)
      , ("default", Json.toJSON t)
      ]

data Parameter =
  Parameter String
            Type
  deriving (Generic, Show, Json.FromJSON)

instance Json.ToJSON Parameter where
  toJSON (Parameter n t) =
    Json.object [("name", Json.toJSON n), ("type", Json.toJSON t)]

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
  toJSON (Primitive txt t) =
    Json.object
      [ ("__tag", "Primitive")
      , ("text", Json.toJSON txt)
      , ("typeName", Json.toJSON t)
      ]
  toJSON (ArrayT txt t) =
    Json.object
      [ ("__tag", "Array")
      , ("text", Json.toJSON txt)
      , ("elementsType", Json.toJSON t)
      ]
  toJSON (Union txt t) =
    Json.object
      [ ("__tag", "Union")
      , ("text", Json.toJSON txt)
      , ("types", Json.toJSON t)
      ]
  toJSON (Intersection txt t) =
    Json.object
      [ ("__tag", "Intersection")
      , ("text", Json.toJSON txt)
      , ("types", Json.toJSON t)
      ]
  toJSON (Tuple txt t) =
    Json.object
      [ ("__tag", "Tuple")
      , ("text", Json.toJSON txt)
      , ("types", Json.toJSON t)
      ]
  toJSON (Function txt signatures) =
    Json.object
      [ ("__tag", "Function")
      , ("text", Json.toJSON txt)
      , ("signatures", Json.toJSON signatures)
      ]
  toJSON (HigherOrder txt t) =
    Json.object
      [ ("__tag", "Function")
      , ("text", Json.toJSON txt)
      , ("arguments", Json.toJSON t)
      ]
  toJSON (Other txt) =
    Json.object [("__tag", "Other"), ("text", Json.toJSON txt)]

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
stringOfType (LiteralString n) = "\"" ++ n ++ "\""
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
  ps <- Comb.sepBy (lexeme type') (lexeme $ C.char ',')
  void $ C.string "=>"
  C.spaces
  -- TODO: parse params -> single leter capitals should be generics
  Signature [] (Parameter "t" <$> ps) <$> type'

simpleType :: Parser Type
simpleType =
  P.try typePrimitive <|>
  P.try typeAny <|>
  P.try typeUnkown <|>
  P.try typeUndefined <|>
  P.try typeBoolLit <|>
  P.try typeStringLit <|>
  P.try typeNumLit <|>
  other

-- TODO: support union, intersection, array & tuple
type' :: Parser Type
type' = tuple

array :: Parser Type
array = simpleType >>= maybeArray
  where
    checkArrSuffix :: Type -> Parser Type
    checkArrSuffix t0 = C.string "[]" *> maybeArray (ArrayT "arr" t0)

    maybeArray :: Type -> Parser Type
    maybeArray t = checkArrSuffix t <|> pure t

-- union' :: Parser Type
-- union' = lexeme simpleType >>= maybeUnion
--   where
--     checkUnion :: Type -> Parser Type
--     checkUnion t = do
--       -- void $ lexeme $ C.char '|'
--       -- t <- lexeme type'
--       ts <- Comb.sepBy1 (lexeme type') (lexeme $ C.char '|')
--       maybeUnion $ Union "arr" (t:ts)

--     maybeUnion :: Type -> Parser Type
--     maybeUnion t = checkUnion t <|> pure t

union :: Parser Type
union = intercalated (Union "u") '|'

intersection :: Parser Type
intersection = intercalated (Intersection "i") '&'

intercalated :: ([Type] -> Type) -> Char -> Parser Type
intercalated constructor sep = P.try intercalated' <|> simpleType
  where
    intercalated' :: Parser Type
    -- TODO: should support type' instead of simpleType
    intercalated' = constructor
                      <$> sepBy2 (lexeme simpleType) (lexeme $ C.char sep)

tuple :: Parser Type
tuple = P.try tuple' <|> simpleType
  where
    ts = Comb.sepBy1 (lexeme simpleType) (lexeme $ C.char ',')
    tuple' = Tuple "t" <$> Comb.between (C.char '[') (C.char ']') ts

checkEnds :: Parser ()
checkEnds = Comb.notFollowedBy C.alphaNum

typeAny :: Parser Type
typeAny = C.string "any" <* checkEnds $> Any

typeUnkown :: Parser Type
typeUnkown = C.string "unkown" <* checkEnds $> Unkown

typeUndefined :: Parser Type
typeUndefined = C.string "undefined" <* checkEnds $> Undefined

typeStringLit :: Parser Type
typeStringLit =
  LiteralString <$> (singleQuoteString <|> doubleQuoteString)

singleQuoteString :: Parser String
singleQuoteString = between (C.char '"') (Comb.many1 $ C.noneOf "\"\n")

doubleQuoteString :: Parser String
doubleQuoteString = between (C.char '\'') (Comb.many1 $ C.noneOf "'\n")

typeBoolLit :: Parser Type
typeBoolLit =
  LiteralBoolean
    <$> ((C.string "true" $> True) <|> (C.string "false" $> False))

typeNumLit :: Parser Type
typeNumLit = LiteralNumber <$> N.floating2 False

typePrimitive :: Parser Type
typePrimitive = Primitive "" <$> primitives

primitives :: Parser PrimitiveType
primitives =
  (C.string "string" $> PString) <|>
  (C.string "number" $> PNumber) <|>
  (C.string "boolean" $> PBool)

other :: Parser Type
other = Other <$> Comb.many1 C.letter 

between :: Parser a -> Parser b -> Parser b
between limit = Comb.between limit limit

whitespace :: Parser ()
whitespace = void $ P.many $ C.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p sep = do
  a <- p
  void sep
  as <- Comb.sepBy1 p sep
  pure (a:as)
