module Apocrypha.Client 
    ( keys, keys'
    , get, get'
    , set, set'
    , getContext
    , Context
    ) 
where

import Apocrypha.Network

keys :: Context -> [String] -> IO [String]
keys con items = do
    result <- client con $ items ++ ["--keys"]
    return $ maybe [] words result

get :: Context -> [String] -> IO (Maybe String)
get = client

set :: Context -> [String] -> String -> IO (Maybe String)
set con items value = 
    client con $ items ++ ["=", value]


-- create a new socket every time versions
keys' :: [String] -> IO [String]
keys' items = do
    result <- client Nothing $ items ++ ["--keys"]
    return $ maybe [] words result

get' :: [String] -> IO (Maybe String)
get' = client Nothing

set' :: [String] -> String -> IO (Maybe String)
set' items value = client Nothing $ items ++ ["=", value]
