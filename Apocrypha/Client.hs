{-# LANGUAGE FlexibleInstances #-}

module Apocrypha.Client
    ( keys, keys'
    , get, get'
    , jGet
    , set, set'
    , del
    , getContext
    , Context
    ) where

import Data.Aeson
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

keys :: Context -> [String] -> IO [String]
keys con items = do
    result <- client con $ items ++ ["--keys"]
    return $ maybe [] words result

get :: Context -> [String] -> IO (Maybe String)
get  = client
jGet = jClient

-- set :: Context -> [String] -> String -> IO ()
-- set con items value = do
--     _ <- client con $ items ++ ["=", value]
--     return ()

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

class (FromJSON a) => Settable a where
    set :: Context -> [String] -> a -> IO ()


instance Settable String where
    set context items value = do
        _ <- client context $ items ++ ["=", value]
        return ()

instance Settable Integer where
    set context items value = do
        _ <- client context $ items ++ ["--set", v]
        return ()
        where v = B8.unpack . B.toStrict . encode $ value
