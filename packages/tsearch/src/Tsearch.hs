{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch where

import Control.Monad (void)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import Data.Default.Class (Default (def))
import Data.Functor (($>))
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Network.HTTP.Types (status400)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.Wai.Middleware.RequestLogger as Log
import Network.Wai.Middleware.RequestLogger.JSON (formatAsJSON)
import Servant
import qualified Servant.Checked.Exceptions as E
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus (toErrStatus))
import Text.Casing (camel)
import qualified Text.Parsec.Char as C
import qualified Text.Parsec.Combinator as Comb
import Text.Parsec.Prim ((<|>))
import qualified Text.Parsec.Prim as P
import Text.Parsec.String (Parser)
import qualified Text.ParserCombinators.Parsec.Number as N

-- SERVER ---
serverMain :: [FunctionRecord] -> Int -> IO ()
serverMain fns port = do
  let settings =
        def {Log.outputFormat = Log.CustomOutputFormatWithDetails formatAsJSON}
  logger <- Log.mkRequestLogger settings
  Warp.run port $ (simpleCors . logger) $ app fns

app :: [FunctionRecord] -> Application
app = serve tsearchAPI . tsearchServer

type TsearchAPI = HelloHandler :<|> SearchHandler

tsearchAPI :: Proxy TsearchAPI
tsearchAPI = Proxy

tsearchServer :: [FunctionRecord] -> Server TsearchAPI
tsearchServer fns = helloHandler :<|> searchHandler fns

type HelloHandler = E.NoThrow :> Get '[JSON] Text

helloHandler :: Handler (E.Envelope '[] Text)
helloHandler = E.pureSuccEnvelope "Hello world"

type SearchHandler =
  "search"
    :> QueryParam "query" String
    :> E.Throws ResponseError
    :> Get '[JSON] [FunctionRecord]

type SearchHandler' = Handler (E.Envelope '[ResponseError] [FunctionRecord])

searchHandler :: [FunctionRecord] -> Maybe String -> SearchHandler'
searchHandler fns (Just q) =
  case P.parse signatureP "" q of
    Right s -> E.pureSuccEnvelope $ find s fns
    Left e -> E.pureErrEnvelope $ InvalidQuery $ show e
searchHandler _ Nothing = E.pureErrEnvelope MissingQuery

data ResponseError
  = MissingQuery
  | InvalidQuery String
  deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

instance ErrStatus ResponseError where
  toErrStatus _ = status400

-- Types ---
dropLabelPrefix :: Int -> Json.Options
dropLabelPrefix n =
  Json.defaultOptions {Json.fieldLabelModifier = camel . drop n}

data Lines
  = Lines
      { from :: Int,
        to :: Int
      }
  deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

data Location
  = Location
      { path :: String,
        lines :: Lines
      }
  deriving (Generic, Show, Json.ToJSON, Json.FromJSON)

data FunctionRecord
  = FunctionRecord
      { frName :: Maybe String,
        frDocs :: Maybe String,
        frText :: Maybe String,
        frLocation :: Location,
        frModule :: String,
        frSignature :: Signature
      }
  deriving (Generic, Show)

instance Json.ToJSON FunctionRecord where
  toJSON = Json.genericToJSON $ dropLabelPrefix 2

instance Json.FromJSON FunctionRecord where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 2

data Module
  = Module
      { mName :: String,
        mFns :: [FunctionRecord]
      }
  deriving (Generic, Show)

instance Json.ToJSON Module where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Module where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data PrimitiveType
  = PNumber
  | PString
  | PBool
  deriving (Generic, Show, Eq, Ord)

instance Json.ToJSON PrimitiveType where
  toJSON PNumber = "number"
  toJSON PString = "string"
  toJSON PBool = "boolean"

instance Json.FromJSON PrimitiveType where
  parseJSON =
    Json.withText "PrimitiveType" $ \case
      "number" -> pure PNumber
      "string" -> pure PString
      "boolean" -> pure PBool
      b -> fail $ "unexpected '" ++ Text.unpack b ++ "'"

data TypeParameter
  = Polymorphic String
  | Constrained String Type
  | WithDefault String Type
  deriving (Generic, Show, Eq, Ord)

instance Json.ToJSON TypeParameter where
  toJSON (Polymorphic n) =
    Json.object ["__tag" .= tpTag (Polymorphic n), "text" .= n]
  toJSON (Constrained n t) =
    Json.object
      ["__tag" .= tpTag (Constrained n t), "text" .= n, "constraint" .= t]
  toJSON (WithDefault n t) =
    Json.object
      ["__tag" .= tpTag (WithDefault n t), "text" .= n, "default" .= t]

instance Json.FromJSON TypeParameter where
  parseJSON =
    Json.withObject "TypeParameter" $ \b -> do
      tag <- b .: "__tag"
      case (tag :: Text) of
        "Constrained" -> Constrained <$> b .: "text" <*> b .: "constraint"
        "WithDefault" -> WithDefault <$> b .: "text" <*> b .: "default"
        "Polymorphic" -> Polymorphic <$> b .: "text"
        t -> fail $ "unknown '__tag' (" ++ Text.unpack t ++ ")"

tpTag :: TypeParameter -> String
tpTag (Polymorphic _) = "Polymorphic"
tpTag (Constrained _ _) = "Constrained"
tpTag (WithDefault _ _) = "WithDefault"

data Parameter
  = Parameter
      { paramName :: String,
        paramType :: Type
      }
  deriving (Generic, Show, Eq, Ord)

instance Json.ToJSON Parameter where
  toJSON = Json.genericToJSON $ dropLabelPrefix 5

instance Json.FromJSON Parameter where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 5

data Signature
  = Signature
      { sTypeParameters :: [TypeParameter],
        sParameters :: [Parameter],
        sReturnType :: Type
      }
  deriving (Generic, Show, Eq, Ord)

instance Json.ToJSON Signature where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Signature where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data Type
  = Any
  | Unknown
  | Undefined
  | LiteralString String
  | LiteralNumber Float
  | LiteralBoolean Bool
  | Primitive String PrimitiveType
  | ArrayT String Type
  | Union String [Type]
  | Intersection String [Type]
  | Tuple String [Type]
  | Function String Signature
  | HigherOrder String [Type]
  | Other String
  deriving (Generic, Show, Eq, Ord)

instance Json.ToJSON Type where
  toJSON Any = Json.object ["__tag" .= typeTag Any]
  toJSON Unknown = Json.object ["__tag" .= typeTag Unknown]
  toJSON Undefined = Json.object ["__tag" .= typeTag Undefined]
  toJSON (LiteralString str) =
    Json.object ["__tag" .= typeTag (LiteralString str), "value" .= str]
  toJSON (LiteralNumber num) =
    Json.object ["__tag" .= typeTag (LiteralNumber num), "value" .= num]
  toJSON (LiteralBoolean bool) =
    Json.object ["__tag" .= typeTag (LiteralBoolean bool), "value" .= bool]
  toJSON (Primitive txt t) =
    Json.object
      ["__tag" .= typeTag (Primitive txt t), "text" .= txt, "typeName" .= t]
  toJSON (ArrayT txt t) =
    Json.object
      ["__tag" .= typeTag (ArrayT txt t), "text" .= txt, "elementsType" .= t]
  toJSON (Union txt ts) =
    Json.object
      ["__tag" .= typeTag (Union txt ts), "text" .= txt, "types" .= ts]
  toJSON (Intersection txt ts) =
    Json.object
      ["__tag" .= typeTag (Intersection txt ts), "text" .= txt, "types" .= ts]
  toJSON (Tuple txt ts) =
    Json.object
      ["__tag" .= typeTag (Tuple txt ts), "text" .= txt, "types" .= ts]
  toJSON (Function txt s) =
    Json.object
      ["__tag" .= typeTag (Function txt s), "text" .= txt, "signature" .= s]
  toJSON (HigherOrder txt ts) =
    Json.object
      [ "__tag" .= typeTag (HigherOrder txt ts),
        "text" .= txt,
        "arguments" .= ts
      ]
  toJSON (Other txt) =
    Json.object ["__tag" .= typeTag (Other txt), "text" .= txt]

instance Json.FromJSON Type where
  parseJSON =
    Json.withObject "Type" $ \b -> do
      tag <- b .: "__tag"
      case (tag :: Text) of
        "Any" -> pure Any
        "Unknown" -> pure Unknown
        "Undefined" -> pure Undefined
        "LiteralString" -> LiteralString <$> b .: "value"
        "LiteralNumber" -> LiteralNumber <$> b .: "value"
        "LiteralBoolean" -> LiteralBoolean <$> b .: "value"
        "Primitive" -> Primitive <$> b .: "text" <*> b .: "typeName"
        "Array" -> ArrayT <$> b .: "text" <*> b .: "elementsType"
        "Union" -> Union <$> b .: "text" <*> b .: "types"
        "Intersection" -> Intersection <$> b .: "text" <*> b .: "types"
        "Tuple" -> Tuple <$> b .: "text" <*> b .: "types"
        "Function" -> Function <$> b .: "text" <*> b .: "signature"
        "HigherOrder" -> HigherOrder <$> b .: "text" <*> b .: "arguments"
        "Other" -> Other <$> b .: "text"
        t -> fail $ "unknown '__tag' (" ++ Text.unpack t ++ ")"

typeTag :: Type -> String
typeTag Any = "Any"
typeTag Unknown = "Unknown"
typeTag Undefined = "Undefined"
typeTag (LiteralString _) = "LiteralString"
typeTag (LiteralNumber _) = "LiteralNumber"
typeTag (LiteralBoolean _) = "LiteralBoolean"
typeTag (Primitive _ _) = "Primitive"
typeTag (ArrayT _ _) = "Array"
typeTag (Union _ _) = "Union"
typeTag (Intersection _ _) = "Intersection"
typeTag (Tuple _ _) = "Tuple"
typeTag (Function _ _) = "Function"
typeTag (HigherOrder _ _) = "HigherOrder"
typeTag (Other _) = "Other"

stringOfPrimitive :: PrimitiveType -> String
stringOfPrimitive PString = "string"
stringOfPrimitive PBool = "boolean"
stringOfPrimitive PNumber = "number"

stringOfParam :: Parameter -> String
stringOfParam (Parameter n t) = n ++ ": " ++ stringOfType t

stringOfParams :: [Parameter] -> String
stringOfParams = List.intercalate ", " . map stringOfParam

stringOfTypeParams :: [TypeParameter] -> String
stringOfTypeParams = List.intercalate ", " . map stringOfTypeParam

stringOfFn :: [Parameter] -> Type -> String
stringOfFn ps rt = "(" ++ stringOfParams ps ++ ") => " ++ stringOfType rt

stringOfSignature :: Signature -> String
stringOfSignature (Signature [] ps rt) = stringOfFn ps rt
stringOfSignature (Signature tps ps rt) =
  "<" ++ stringOfTypeParams tps ++ ">" ++ stringOfFn ps rt

stringOfTypeParam :: TypeParameter -> String
stringOfTypeParam (Polymorphic s) = s
stringOfTypeParam (Constrained s t) = s ++ " extends " ++ stringOfType t
stringOfTypeParam (WithDefault s d) = s ++ " = " ++ stringOfType d

stringOfType :: Type -> String
stringOfType Any = "any"
stringOfType Unknown = "unknown"
stringOfType Undefined = "undefine"
stringOfType (LiteralString n) = "\"" ++ n ++ "\""
stringOfType (LiteralNumber num) = show num
stringOfType (LiteralBoolean True) = "true"
stringOfType (LiteralBoolean False) = "false"
stringOfType (Primitive _ t) = stringOfPrimitive t
stringOfType (ArrayT _ t) = stringOfType t ++ "[]"
stringOfType (Union _ ts) = List.intercalate " | " $ map stringOfType ts
stringOfType (Intersection _ ts) = List.intercalate " & " $ map stringOfType ts
stringOfType (Tuple _ ts) =
  "[" ++ List.intercalate " & " (map stringOfType ts) ++ "]"
stringOfType (Function _ s) = stringOfSignature s
stringOfType (HigherOrder t as) =
  t ++ "<" ++ (List.intercalate ", " . map stringOfType $ as) ++ ">"
stringOfType (Other s) = s

-- Search ---

-- dummy search: the fist 100 cases that arity matches (+/- 1)
find :: Signature -> [FunctionRecord] -> [FunctionRecord]
find signature = take 100 . filter (checkArity qArity)
  where
    qArity = arity signature

checkArity :: Int -> FunctionRecord -> Bool
checkArity qArity fr =
  (frArity + 1) == qArity || (frArity - 1) == qArity || frArity == qArity
  where
    frArity = arity $ frSignature fr

arity :: Signature -> Int
arity = length . sParameters

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
  P.try typePrimitive
    <|> P.try typeAny
    <|> P.try typeUnknown
    <|> P.try typeUndefined
    <|> P.try typeBoolLit
    <|> P.try typeStringLit
    <|> P.try typeNumLit
    <|> other

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
--       maybeUnion $ Union "arr" (t : ts)
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
    intercalated' =
      constructor <$> sepBy2 (lexeme simpleType) (lexeme $ C.char sep)

tuple :: Parser Type
tuple = P.try tuple' <|> simpleType
  where
    ts = Comb.sepBy1 (lexeme simpleType) (lexeme $ C.char ',')
    tuple' = Tuple "t" <$> Comb.between (C.char '[') (C.char ']') ts

checkEnds :: Parser ()
checkEnds = Comb.notFollowedBy C.alphaNum

typeAny :: Parser Type
typeAny = C.string "any" <* checkEnds $> Any

typeUnknown :: Parser Type
typeUnknown = C.string "unknown" <* checkEnds $> Unknown

typeUndefined :: Parser Type
typeUndefined = C.string "undefined" <* checkEnds $> Undefined

typeStringLit :: Parser Type
typeStringLit = LiteralString <$> (singleQuoteString <|> doubleQuoteString)

singleQuoteString :: Parser String
singleQuoteString = between (C.char '"') (Comb.many1 $ C.noneOf "\"\n")

doubleQuoteString :: Parser String
doubleQuoteString = between (C.char '\'') (Comb.many1 $ C.noneOf "'\n")

typeBoolLit :: Parser Type
typeBoolLit =
  LiteralBoolean <$> ((C.string "true" $> True) <|> (C.string "false" $> False))

typeNumLit :: Parser Type
typeNumLit = LiteralNumber <$> N.floating2 False

typePrimitive :: Parser Type
typePrimitive = Primitive "" <$> primitives

primitives :: Parser PrimitiveType
primitives =
  (C.string "string" $> PString)
    <|> (C.string "number" $> PNumber)
    <|> (C.string "boolean" $> PBool)

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
  pure (a : as)
