{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Apocrypha.Internal.Cache
    Description : Simple caching primitives
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

module Apocrypha.Internal.Cache
    ( Cache
    , emptyCache
    , cacheGet, cachePut
    ) where

import           Apocrypha.Database  (Query)

import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)


type Cache = HM.HashMap Query Text
-- ^ a quick lookup data structure for read queries


emptyCache :: Cache
-- ^ a cache with no elements
emptyCache = HM.fromList []


cacheGet :: Cache -> Query -> Maybe Text
-- ^ try to retrieve an element from a cache
cacheGet cache query = HM.lookup query cache


cachePut :: Cache -> Query -> Text -> Cache
-- ^ place an element in the cache if it was a read query
cachePut cache query value
        | isReadQuery query = HM.insert query value cache
        | otherwise         = cache


-- | Internal

isReadQuery :: Query -> Bool
isReadQuery = not . any (`elem` writeOps)


writeOps :: [Text]
writeOps = [ "-s", "--set"
           , "-d", "--del"
           , "-p", "--pop"
           , "+", "-", "="]
