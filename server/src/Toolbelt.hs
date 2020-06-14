module Toolbelt
  ( regularParse,
    listJoin,
    stripEnd,
    dropLabelPrefix,
  )
where

import qualified Data.Aeson as Json
import Data.Foldable (fold)
import Data.List (dropWhileEnd, intersperse)
import Text.Casing (camel)
import Text.Parsec
import Text.Parsec.String

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = Text.Parsec.parse p "regularParse"

listJoin :: [a] -> [[a]] -> [a]
listJoin what = fold . intersperse what

stripEnd :: String -> String
stripEnd = dropWhileEnd (== ' ')

dropLabelPrefix :: Int -> Json.Options
dropLabelPrefix n =
  Json.defaultOptions {Json.fieldLabelModifier = camel . drop n}
