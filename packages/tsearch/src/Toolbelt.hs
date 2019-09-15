module Toolbelt where

import Text.Parsec
import Text.Parsec.String

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = Text.Parsec.parse p "regularParse"
