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
    , get, put
    ) where

import           Apocrypha.Database  (Query)

import qualified Data.HashMap.Strict as HM
import           Data.Text           (Text)


type Cache = HM.HashMap Query Text


emptyCache :: Cache
emptyCache = HM.fromList []


get :: Bool -> Cache -> Query -> Maybe Text
get True cache query = HM.lookup query cache
get False _ _        = Nothing


put :: Cache -> Query -> Text -> Cache
put cache query value
        | isReadQuery query = HM.insert query value cache
        | otherwise         = cache


-- | Internal

isReadQuery :: Query -> Bool
-- ^ only read requests can be cached
isReadQuery = not . any (`elem` writeOps)


writeOps :: [Text]
writeOps = [ "-s", "--set"
           , "-d", "--del"
           , "-p", "--pop"
           , "+", "-", "="]
