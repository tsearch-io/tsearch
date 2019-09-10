{-# LANGUAGE OverloadedStrings #-}

module Tsearch.Query
  ( signature
  ) where

import           Data.Functor                         (($>))
import qualified Text.Parsec.Char                     as C
import qualified Text.Parsec.Combinator               as Comb
import           Text.Parsec.Prim                     ((<?>), (<|>))
import qualified Text.Parsec.Prim                     as P
import           Text.Parsec.String                   (Parser)
import qualified Text.ParserCombinators.Parsec.Number as N
import           Tsearch.Types                        (Parameter (..),
                                                       PrimitiveType (..),
                                                       Signature (..),
                                                       Type (..))
 -- keywordLet  = try (do{ string "let"
 --                      ; notFollowedBy alphaNum
 --                      })

signature :: Parser Signature
signature = do
  C.spaces
  params <- Comb.sepBy (C.spaces *> type' <* C.spaces) (C.char ',')
  C.string "=>"
  C.spaces
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
singleQuoteString =
  Comb.between (C.char '"') (C.char '"') (Comb.many1 $ C.noneOf "\"\n")

doubleQuoteString :: Parser String
doubleQuoteString =
  Comb.between (C.char '\'') (C.char '\'') (Comb.many1 $ C.noneOf "'\n")

typeBoolLit :: Parser Type
typeBoolLit =
  LiteralBoolean <$>
  (P.try (C.string "true" $> True) <|> (C.string "false" $> False))

typeNumLit :: Parser Type
typeNumLit = LiteralNumber <$> N.floating2 False

typePrimitive :: Parser Type
typePrimitive =
  Primitive "" <$>
  (P.try (C.string "string" $> PString) <|> P.try (C.string "number" $> PNumber) <|>
   (C.string "boolean" $> PBool))
