{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Devbot.Core where

import GHC.Generics
import Apocrypha.Client

import qualified Data.Map
import Data.Aeson

import Data.Maybe (fromMaybe)
import Control.Monad (liftM2, liftM3, liftM4)
import Text.Read (readMaybe)


data Event = Event Config Data
    deriving (Show, Eq)

-- data Data = Data Duration When Errors
data Data = Data
          { duration :: Integer
          , when   :: Integer
          , errors :: Maybe Integer
          }
    deriving (Show, Eq, Generic)

instance FromJSON Data where
    parseJSON = withObject "Data" $ \v -> Data
        <$> v .:  "duration"
        <*> v .:  "when"
        <*> v .:? "errors"

instance ToJSON Data where
    toEncoding = genericToEncoding defaultOptions

type Duration = Integer
type When = Integer
type Errors = Maybe Integer

data Config = Config Name Action Interval Require
    deriving (Eq, Show)
type Name = String
type Action = [String]
type Interval = String
type Require = Maybe String

devbot :: Context -> [String] -> IO (Maybe String)
devbot context items =
    get context $ "devbot" : items

maybeInt :: Maybe String -> Maybe Integer
maybeInt = maybe Nothing (\y -> readMaybe y :: Maybe Integer)

getData :: Context -> String -> IO (Maybe Data)
getData context event = do
    let c = context

    m <- jGet c ["devbot", "data", event, "--edit"]
    return (decode m :: Maybe Data)

    -- d <- devbot c ["data", event, "duration"]
    -- w <- devbot c ["data", event, "when"]
    -- r <- devbot c ["data", event, "errors"]

    -- let duration = maybeInt d
    --     when     = maybeInt w
    --     errors   = Just (maybeInt r)

    -- -- return $ liftM3 Data duration when errors
    -- return Nothing

getConfig :: Context -> String -> IO (Maybe Config)
getConfig context event = do
    let c = context

    a <- devbot c ["events", event, "action"]
    i <- devbot c ["events", event, "interval"]
    r <- devbot c ["events", event, "require"]

    let name     = Just event
        action   = fmap lines a
        interval = i
        require  = Just r

    return $ liftM4 Config name action interval require

getEvent :: Context -> String -> IO (Maybe Event)
getEvent context event = do
    -- it's fine for Data to be missing, use a default when it is
    c <- getConfig context event
    d <- getData context event

    return $ liftM2 Event c (case d of
                                Nothing  -> defaultData
                                (Just _) -> d)

    where defaultData = Nothing -- Just $ Data 0 0 Nothing

events :: IO [Maybe Event]
events = do
    context <- getContext Nothing Nothing -- (Just "aspen.anardil.net") Nothing
    keys context ["devbot", "events"] >>= mapM (getEvent context)
