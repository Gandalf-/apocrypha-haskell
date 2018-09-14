module Apocrypha.Client
    ( keys, keys'
    , get, get'
    , set, set'
    , del
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

set :: Context -> [String] -> String -> IO ()
set con items value = do
    _ <- client con $ items ++ ["=", value]
    return ()

del :: Context -> [String] -> IO ()
del con items = do
    _ <- client con $ items ++ ["--del"]
    return ()

-- create a new socket every time versions
keys' :: [String] -> IO [String]
keys' items = do
    result <- client Nothing $ items ++ ["--keys"]
    return $ maybe [] words result

get' :: [String] -> IO (Maybe String)
get' = client Nothing

set' :: [String] -> String -> IO (Maybe String)
set' items value = client Nothing $ items ++ ["=", value]
