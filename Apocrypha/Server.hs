{-# LANGUAGE OverloadedStrings #-}

module Apocrypha.Server where

import Data.Maybe
import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import           Data.Text                   (Text)

import           Apocrypha.Internal.Database (Query)

import Apocrypha.Server.Plugin.Hasher
import Apocrypha.Server.Plugin


runPlugin :: Value -> Query -> IO (Maybe (Text, Bool, Value))
runPlugin db (".plugin": "hasher": q) =
        Just <$> execute h hDb q
    where
        hDb = getPluginDb db "hasher"
        h = Hasher

runPlugin _ _ = pure Nothing

getPluginDb :: Value -> Text -> Value
-- extract or create a sub database for the plugin
getPluginDb db t =
        fromMaybe Null $ go db
    where
        go (Object o) = do
            s <- HM.lookup ".plugin" o
            case s of
                (Object s') -> HM.lookup t s'
                _           -> Nothing
        go _ = Nothing
