{-# LANGUAGE OverloadedStrings #-}

module Apocrypha.Cache
    ( Cache
    , emptyCache
    , get, put
    ) where

import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)

import           Apocrypha.Database  (Query)
import           Data.Aeson


-- | Exposed

type Cache = HM.HashMap Query Text


emptyCache :: Cache
emptyCache = HM.fromList []

get :: Cache -> Query -> Maybe Text
get cache query = HM.lookup query cache


put :: Cache -> Query -> Text -> Cache
put cache query value
        | isReadQuery query = HM.insert query value cache
        | otherwise         = cache


-- | Internal

isReadQuery :: Query -> Bool
-- ^ only read requests can be cached
isReadQuery = not . any (`elem` writeOps)


writeOps :: [Text]
writeOps = ["-s", "--set"
           , "-d", "--del"
           , "--pop"
           , "+", "-", "="]
