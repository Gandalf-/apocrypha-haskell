module Apocrypha.Client 
    ( keys
    , get
    , set
    ) 
where

import Apocrypha.Network

keys :: [String] -> IO [String]
keys items = do
    result <- client $ items ++ ["--keys"]
    return $ maybe [] words result


get :: [String] -> IO (Maybe String)
get = client


set :: [String] -> String -> IO (Maybe String)
set items value = client $ items ++ ["=", value]
