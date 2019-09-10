{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tsearch.Types
  ( stringOfType
  , signatureToString
  , Type(..)
  , Parameter(..)
  , PrimitiveType(..)
  , TypeParameter(..)
  , Signature(..)
  , FunctionRecord(..)
  ) where

import           Data.Aeson   ((.=))
import qualified Data.Aeson   as Json
import qualified Data.List    as List
import           GHC.Generics

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

fn :: [Parameter] -> Type -> String
fn ps rt = "(" ++ params ps ++ ") => " ++ stringOfType rt

signatureToString :: Signature -> String
signatureToString (Signature [] ps rt) = fn ps rt
signatureToString (Signature tps ps rt) =
  "<" ++ typeParams tps ++ ">" ++ fn ps rt

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
stringOfType (Function _ []) = ""
stringOfType (Function _ (s:_)) = signatureToString s
stringOfType (HigherOrder t as) =
  t ++ "<" ++ (List.intercalate ", " . map stringOfType $ as) ++ ">"
stringOfType (Other s) = s
