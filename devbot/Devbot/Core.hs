{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Devbot.Core where

import           Data.Aeson
import           Data.Foldable       (asum)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Read           (readMaybe)

import           Apocrypha.Client


-- | Event
-- wrapper for the data stored in 'data' and 'events'

data Event = Event
       { _name   :: !String
       , _config :: !Config
       , _data   :: !Data
       }
    deriving (Show, Eq)


-- | Data
-- run time information for a devbot action, populated by devbot

data Data = Data
        { duration :: !Integer
        , when     :: !Integer
        , errors   :: !(Maybe Integer)
        }
    deriving (Show, Eq, Generic)

instance FromJSON Data where

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions


-- | Config
-- devbot action specification, comes from config file

data Config = Config
        { action   :: ![String]
        , interval :: !Integer
        , require  :: !(Maybe String)
        }
    deriving (Eq, Show, Generic)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        -- action may be a string or list of string
        action   <- asum [
            do a <- o .: "action"
               case a of
                   (Array _)             -> parseJSON a
                   (Data.Aeson.String s) -> return [T.unpack s]
                   _                     -> fail ""
            ]

        -- interval may be a string or number
        interval <- asum [
            do i <- o .: "interval"
               case i of
                   (Number _)            -> parseJSON i
                   (Data.Aeson.String s) -> return . parse . T.unpack $ s
                   _                     -> fail ""
            ]

        require  <- o .:? "require"

        return Config{..}
      where
          parse :: String -> Integer
          parse "hourly" = hour
          parse "daily"  = daily
          parse "weekly" = weekly
          parse n        = fromMaybe daily (readMaybe n :: Maybe Integer)

          weekly = daily * 7
          daily  = hour * 24
          hour   = minute * 60
          minute = 60

instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions


-- | Library

type ConfigMap = HM.HashMap String Config
type DataMap   = HM.HashMap String Data


events :: IO [Event]
-- ^ retrieve all events stored in the database
events = do
    c <- defaultContext
    cs <- get c ["devbot", "events"] :: IO (Maybe ConfigMap)
    ds <- get c ["devbot", "data"  ] :: IO (Maybe DataMap)

    let configs = HM.toList . fromMaybe (HM.fromList []) $ cs
        datas = fromMaybe (HM.fromList []) ds

        parse :: (String, Config) -> Event
        parse (name, config) =
            Event name config . fromMaybe defaultData $ HM.lookup name datas

    return $ map parse configs
    where
          defaultData = Data 0 0 Nothing
