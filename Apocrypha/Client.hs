{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      : Apocrypha.Client
    Description : Client facing API to an Apocrypha server
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

module Apocrypha.Client
    ( keys,  get,  set,  del , pop , append
    , keys', get', set', del', pop', append'
    , Context, getContext, defaultContext, getServerlessContext, getMemoryContext
    ) where

import           Apocrypha.Protocol

import           Control.Monad         (void)
import           Data.Aeson
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy  (toStrict)


withContext :: (Context -> Query -> IO a) -> Query -> IO a
-- ^ run a simple client function with the default context
withContext f q = do
        cx <- defaultContext
        f cx q


keys :: Context -> Query -> IO [String]
-- ^ retrieve the keys for a dictionary
keys c items = do
        result <- client c $ items <> ["--keys"]
        pure $ maybe [] words result

keys' :: Query -> IO [String]
-- ^ retrieve the keys for a dictionary
keys' = withContext keys


del :: Context -> Query -> IO ()
-- ^ delete a value of any type
del c items =
        void $ client c $ items <> ["--del"]

del' :: Query -> IO ()
-- ^ delete a value of any type
del' = withContext del


set :: (ToJSON a) => Context -> Query -> a -> IO ()
-- ^ assign a value of any type
set context items value =
        void $ client context $ items <> ["--set", v]
    where
        v = unpack . toStrict $ encode value

set' :: (ToJSON a) => Query -> a -> IO ()
-- ^ assign a value of any type
set' items value =
        defaultContext >>= (\ c -> set c items value)


get :: (FromJSON a) => Context -> Query -> IO (Maybe a)
-- ^ retrieve a value of any type
get context items = do
        result <- jClient context $ items <> ["--edit"]
        pure $ case result of
            Just m  -> decode m :: (FromJSON a) => Maybe a
            Nothing -> Nothing

get' :: (FromJSON a) => Query -> IO (Maybe a)
-- ^ retrieve a value of any type
get' items =
        defaultContext >>= (`get` items)


append :: Context -> Query -> String -> IO ()
-- ^ append a string to a array of strings
append context items value =
        void $ client context $ items <> ["+", value]

append' :: Query -> String -> IO ()
-- ^ append a string to a array of strings
append' items value =
        defaultContext >>= (\ c -> append c items value)


pop :: Context -> Query -> IO (Maybe String)
-- ^ remove and return the first element from an array
pop context items =
        client context $ items <> ["--pop"]

pop' :: Query -> IO (Maybe String)
-- ^ remove and return the first element from an array
pop' = withContext pop
