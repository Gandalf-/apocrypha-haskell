module Devbot.Core 
--     ( Event (..)
--     , Config (..)
--     , Data (..)
--     , events
--     ) 
where

import Control.Monad (liftM2, liftM3, liftM4)
import Apocrypha.Client
import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)


data Event = Event Config Data
    deriving (Eq)

data Data = Data Duration When Errors
    deriving (Show, Eq)
type Duration = Integer
type When = Integer
type Errors = Maybe Integer

data Config = Config Name Action Interval Require
    deriving (Eq, Show)
type Name = String
type Action = [String]
type Interval = String
type Require = Maybe String


devbot xs = get $ "devbot" : xs
maybeInt = maybe Nothing (\y -> readMaybe y :: Maybe Integer)

getData :: String -> IO (Maybe Data)
getData event = do
    d <- devbot ["data", event, "duration"]
    w <- devbot ["data", event, "when"]
    r <- devbot ["data", event, "errors"]

    let duration = maybeInt d
        when     = maybeInt w
        errors   = Just (maybeInt r)

    return $ liftM3 Data duration when errors

getConfig :: String -> IO (Maybe Config)
getConfig event = do
    a <- devbot ["events", event, "action"]
    i <- devbot ["events", event, "interval"]
    r <- devbot ["events", event, "require"]

    let name     = Just event
        action   = fmap lines a
        interval = i
        require  = Just r

    return $ liftM4 Config name action interval require

getEvent :: String -> IO (Maybe Event)
getEvent event = do
    c <- getConfig event
    d <- getData event
    return $ liftM2 Event c d

events :: IO [Maybe Event]
events = keys ["devbot", "events"] >>= mapM getEvent
