{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Tsearch where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

data TsFunc = TsFunc
  { name :: String,
    args :: [TsType],
    return :: TsType
  }
  deriving (Show, Eq)

type TsType = String

type ID = String

type Index = Map ID TsFunc

data Arg = Arg
  { type_ :: TsType,
    next :: [Arg], -- Next argument
    returns :: [Return] -- Return type
  }
  deriving (Show, Eq)

data Return = Return
  { type_ :: TsType,
    ids :: [ID] -- Ref to the function (Map ID Fn)
  }
  deriving (Show, Eq)

newtype TsTrie = TsTrie [Arg]

data TsSignature = TsSignature
  { args :: [TsType],
    return :: TsType
  }
  deriving (Show, Eq)

-- Generated at build time

index :: Index
index =
  Map.fromList
    [ ("123", TsFunc "replace" ["string", "RegExp"] "string"),
      ("456", TsFunc "length" ["string"] "number"),
      ("789", TsFunc "len" ["string"] "number"),
      ("987", TsFunc "volume" ["number", "number", "number"] "number"),
      ("654", TsFunc "area" ["number", "number"] "number")
    ]

trie :: TsTrie
trie =
  TsTrie
    [ Arg
        "string"
        [Arg "RegExp" [] [Return "string" ["123"]]]
        [Return "number" ["456", "789"]],
      Arg
        "number"
        [ Arg
            "number"
            [Arg "number" [] [Return "number" ["987"]]]
            [Return "number" ["654"]]
        ]
        []
    ]

-- Search queries

-- (string) => (RegExp) => number
s1 = TsSignature ["string", "RegExp"] "string"

-- (number) => (number) => (number) => number
s2 = TsSignature ["number", "number", "number"] "number"

-- (string) => number
s3 = TsSignature ["string"] "number"

searchBySignature :: TsTrie -> TsSignature -> [ID]
searchBySignature (TsTrie []) _ = []
searchBySignature (TsTrie trie) query =
  walkQuery query trie []

walkQuery :: TsSignature -> [Arg] -> [Return] -> [ID]
walkQuery (TsSignature [] return) _ returns =
  maybe [] (\ret -> ret.ids) $
    List.find (\(Return type_ ids) -> type_ == return) $
      returns
walkQuery (TsSignature (arg : next) return) args returns =
  case mbNextArg of
    Nothing -> []
    Just (Arg _ args returns) -> walkQuery (TsSignature next return) args returns
  where
    mbNextArg = List.find (\(Arg type_ args returns) -> arg == type_) args

search :: Index -> TsTrie -> TsSignature -> [TsFunc]
search i t q =
  catMaybes $ map (`Map.lookup` i) $ Tsearch.searchBySignature t q
