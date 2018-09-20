{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Devbot.Core where

import GHC.Generics
import Apocrypha.Client

import qualified Data.Map
import Data.Aeson

import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM2, liftM3, liftM4)
import Text.Read (readMaybe)
import qualified Data.Text as T


data Event = Event Name Config Data
    deriving (Show, Eq)

type Name = String

data Data = Data
          { duration :: Integer
          , when   :: Integer
          , errors :: Maybe Integer
          }
    deriving (Show, Eq, Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions

data Config = Config
            { action :: [String]
            , interval :: String
            , require :: Maybe String
            }
    deriving (Eq, Show, Generic)

data Action = Action [String] | String
    deriving (Show, Eq, Generic)

instance FromJSON Action where

instance ToJSON Action where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        action   <- asum [
                do s <- o .: "action"
                   case s of
                       (Array _)             -> parseJSON s
                       (Data.Aeson.String s) -> return [T.unpack s]
            ]
        interval <- o .:  "interval"
        require  <- o .:? "require"

        return Config{..}

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


devbot :: Context -> [String] -> IO (Maybe String)
devbot context items =
    get context $ "devbot" : items


getEvent :: Context -> String -> IO (Maybe Event)
getEvent context name = do
    -- it's fine for Data to be missing, use a default when it is

    c <- getter context ["devbot", "events", name] :: IO (Maybe Config)
    d <- getter context ["devbot", "data"  , name] :: IO (Maybe Data)

    return $ liftM3 Event
        (Just name)
        c
        (case d of
           Nothing  -> defaultData
           (Just _) -> d)

    where defaultData = Just $ Data 0 0 Nothing

events :: IO [Maybe Event]
events = do
    context <- getContext Nothing Nothing -- (Just "aspen.anardil.net") Nothing
    keys context ["devbot", "events"] >>= mapM (getEvent context)
