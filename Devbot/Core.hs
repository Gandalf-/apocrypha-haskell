module Devbot.Core where

import Apocrypha.Client

import Control.Monad (liftM2, liftM3, liftM4)
import Text.Read (readMaybe)


data Event = Event Config Data
    deriving (Show, Eq)

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

devbot c xs = get c $ "devbot" : xs
maybeInt = maybe Nothing (\y -> readMaybe y :: Maybe Integer)

getData :: Context -> String -> IO (Maybe Data)
getData context event = do
    let c = context

    d <- devbot c ["data", event, "duration"]
    w <- devbot c ["data", event, "when"]
    r <- devbot c ["data", event, "errors"]

    let duration = maybeInt d
        when     = maybeInt w
        errors   = Just (maybeInt r)

    return $ liftM3 Data duration when errors

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
    c <- getConfig context event
    d <- getData context event

    return $ liftM2 Event c d

events :: IO [Maybe Event]
events = do
    context <- getContext
    keys' ["devbot", "events"] >>= mapM (getEvent context)
