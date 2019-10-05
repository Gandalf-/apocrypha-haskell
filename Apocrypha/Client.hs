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
    , Context, connect, connectPath, connectNet, defaultConnect
    , TcpConnection, UnixConnection, Serverless, CachingServerless, MemoryDB
    ) where

import           Apocrypha.Protocol

import           Control.Monad         (void)
import           Data.Aeson
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy  (toStrict)


defaultConnect :: IO (Maybe TcpConnection)
defaultConnect = connectNet

keys :: Context a => a -> Query -> IO [String]
-- ^ retrieve the keys for a dictionary
keys c items = do
        result <- client c $ items <> ["--keys"]
        pure $ maybe [] words result


del :: Context a => a -> Query -> IO ()
-- ^ delete a value of any type
del c items =
        void $ client c $ items <> ["--del"]

set :: (ToJSON a, Context b) => b -> Query -> a -> IO ()
-- ^ assign a value of any type
set context items value =
        void $ client context $ items <> ["--set", v]
    where
        v = unpack . toStrict $ encode value

get :: (FromJSON a, Context b) => b -> Query -> IO (Maybe a)
-- ^ retrieve a value of any type
get context items = do
        result <- jClient context $ items <> ["--edit"]
        pure $ case result of
            Just m  -> decode m :: (FromJSON a) => Maybe a
            Nothing -> Nothing


append :: Context a => a -> Query -> String -> IO ()
-- ^ append a string to a array of strings
append context items value =
        void $ client context $ items <> ["+", value]


pop :: Context a => a -> Query -> IO (Maybe String)
-- ^ remove and return the first element from an array
pop context items =
        client context $ items <> ["--pop"]
