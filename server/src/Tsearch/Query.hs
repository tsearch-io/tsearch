{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tsearch.Query
  ( Lines (..),
    Location (..),
    FunctionRecord (..),
    Module (..),
    Signature (..),
    Param (..),
    Type (..),
    showType,
    Query (..),
    showQuery,
    query,
  )
where

import Control.Monad (void)
import qualified Data.Aeson as Json
import GHC.Generics
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import qualified Text.ParserCombinators.Parsec.Number as N
import Toolbelt (dropLabelPrefix, listJoin, stripEnd)

-- Types ---

data Lines = Lines
  { lFrom :: Integer,
    lTo :: Integer
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Lines where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Lines where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data Location = Location
  { locPath :: String,
    locLines :: Lines
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Location where
  toJSON = Json.genericToJSON $ dropLabelPrefix 3

instance Json.FromJSON Location where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 3

data FunctionRecord = FunctionRecord
  { frName :: Maybe String,
    frDocs :: Maybe String,
    frText :: Maybe String,
    frLocation :: Location,
    frModule :: String,
    frSignature :: Signature
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON FunctionRecord where
  toJSON = Json.genericToJSON $ dropLabelPrefix 2

instance Json.FromJSON FunctionRecord where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 2

data Module = Module
  { mName :: String,
    mFns :: [FunctionRecord]
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Module where
  toJSON = Json.genericToJSON $ dropLabelPrefix 1

instance Json.FromJSON Module where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 1

data Signature = Signature
  { sigtParameters :: [Param],
    sigtReturnType :: Type
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Signature where
  toJSON = Json.genericToJSON $ dropLabelPrefix 4

instance Json.FromJSON Signature where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 4

data Param = Param
  { paramName :: String,
    paramType :: Type
  }
  deriving (Generic, Show, Eq)

instance Json.ToJSON Param where
  toJSON = Json.genericToJSON $ dropLabelPrefix 5

instance Json.FromJSON Param where
  parseJSON = Json.genericParseJSON $ dropLabelPrefix 5

data Type
  = Any
  | Unknown
  | Null
  | Undefined
  | Void
  | Never
  | BoolT
  | StringT
  | NumberT
  | LiteralString String
  | LiteralNumber Float
  | LiteralBool Bool
  | Union [Type] -- !!!
  | Fn [Type] Type
  | Generic Int Int -- !!!
  | ArrayT Type
  | HigherOrder1 String Type
  | HigherOrderN String [Type]
  | Named String
  | Other String
  deriving (Generic, Show, Eq)

showType :: Type -> String
showType Any = "any"
showType Unknown = "unknown"
showType Undefined = "undefined"
showType Null = "null"
showType Void = "void"
showType Never = "never"
showType BoolT = "boolean"
showType StringT = "string"
showType NumberT = "number"
showType (LiteralString str) = "'" ++ str ++ "'"
showType (LiteralNumber n) = show n
showType (LiteralBool True) = "true"
showType (LiteralBool False) = "false"
showType (Union ts) = listJoin " | " $ fmap showType ts
showType (Fn args ret) = "(" ++ ps ++ ") => " ++ showType ret
  where
    param (t, c) = [c] ++ ": " ++ showType t
    ps = listJoin ", " (param <$> zip args ['a' ..])
showType (Generic 0 i) = [['A' ..] !! i]
showType (Generic lvl i) = ['A' ..] !! i : show lvl
showType (ArrayT t) = "Array<" ++ showType t ++ ">"
showType (HigherOrder1 name t) = name ++ "<" ++ showType t ++ ">"
showType (HigherOrderN name ts) =
  name ++ "<" ++ listJoin ", " (fmap showType ts) ++ ">"
showType (Named name) = name
showType (Other str) = str

instance Json.ToJSON Type where
  toJSON = Json.genericToJSON typeOptions

instance Json.FromJSON Type where
  parseJSON = Json.genericParseJSON typeOptions

sumEncoding :: Json.SumEncoding
sumEncoding =
  Json.TaggedObject
    { Json.tagFieldName = "__tag",
      Json.contentsFieldName = "values"
    }

typeOptions :: Json.Options
typeOptions =
  Json.defaultOptions
    { Json.tagSingleConstructors = True,
      Json.sumEncoding = sumEncoding
    }

-- Query Parsing

data Query
  = ByName String
  | BySignature Signature
  deriving (Show, Eq)

showQuery :: Query -> String
showQuery (ByName name) = name
showQuery (BySignature (Signature [] ret)) = "() => " ++ showType ret
showQuery (BySignature (Signature ps ret)) =
  listJoin ", " (fmap (showType . paramType) ps) ++ " => " ++ showType ret

query :: Parser Query
query = P.try byName <|> bySignature

byName :: Parser Query
byName = ByName <$> (whitespace *> lexeme varName <* P.eof)

bySignature :: Parser Query
bySignature =
  BySignature
    <$> ( Signature
            <$> (nameParams <$> lexeme params) <* lexeme (C.string "=>")
            <*> lexeme type_ <* P.eof
        )

nameParams :: [Type] -> [Param]
nameParams ts = addName <$> zip ts ['a' ..]
  where
    addName (t, c) = Param [c] t

params :: Parser [Type]
params =
  P.sepBy (lexeme type_) (lexeme $ P.char ',')

type_ :: Parser Type
type_ =
  P.try (Any <$ keyword "any")
    <|> P.try (Unknown <$ keyword "unknown")
    <|> P.try (Undefined <$ keyword "undefined")
    <|> P.try (Null <$ keyword "null")
    <|> P.try (Void <$ keyword "void")
    <|> P.try (Never <$ keyword "never")
    <|> P.try (BoolT <$ keyword "boolean")
    <|> P.try (StringT <$ keyword "string")
    <|> P.try (NumberT <$ keyword "number")
    <|> P.try (LiteralBool True <$ keyword "true")
    <|> P.try (LiteralBool False <$ keyword "false")
    <|> P.try (LiteralNumber <$> N.floating <* P.notFollowedBy C.alphaNum)
    <|> P.try (LiteralString <$> between (C.char '"') (P.many $ C.noneOf "\"\n"))
    <|> P.try (LiteralString <$> between (C.char '\'') (P.many $ C.noneOf "'\n"))
    <|> (Other . stripEnd <$> P.many1 (C.noneOf "=,\n\t"))

varName :: Parser String
varName = lexeme ((:) <$> firstChar <*> P.many nonFirstChar)
  where
    firstChar = C.letter <|> C.char '_'
    nonFirstChar = C.digit <|> firstChar

whitespace :: Parser ()
whitespace = void $ P.many $ C.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

between :: Parser a -> Parser b -> Parser b
between p = P.between p p

keyword :: String -> Parser String
keyword str = C.string str <* P.notFollowedBy C.alphaNum
