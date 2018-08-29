module Devbot.Core where

import Apocrypha.Client

import Text.Read (readMaybe)


data Event = Event 
           { eventConfig :: Config
           , eventData :: Data
           } deriving (Show, Eq)

data Data = Data
          { duration :: Integer
          , when :: Integer
          , errors :: Maybe Integer
          } deriving (Show, Eq)

data Config = Config 
            { name :: String
            , action :: [String]
            , interval :: String
            , require :: Maybe String 
            } deriving (Eq, Show)


toData :: Maybe String -> Maybe String -> Maybe String -> Maybe Data
toData Nothing _ _ = Nothing
toData _ Nothing _ = Nothing
toData (Just d) (Just w) r = parse parsedD parsedW parsedR
  where 
        parse :: Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Data
        parse Nothing _ _ = Nothing
        parse _ Nothing _ = Nothing
        parse (Just d') (Just w') r' = Just (Data d' w' r')

        parsedD = readMaybe d :: Maybe Integer
        parsedW = readMaybe w :: Maybe Integer
        parsedR = maybe Nothing (\x -> readMaybe x :: Maybe Integer) r

toConfig :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe Config
toConfig _ Nothing _ _ = Nothing
toConfig _ _ Nothing _ = Nothing
toConfig n (Just a) (Just i) r = Just (Config n (lines a) i r)


getData :: String -> IO (Maybe Data)
getData e = do
    d <- grab "duration"
    w <- grab "when"
    r <- grab "errors"
    return $ toData d w r
    where grab s = get ["devbot", "data", e, s]

getConfig :: String -> IO (Maybe Config)
getConfig e = do
    a <- grab "action"
    i <- grab "interval"
    r <- grab "require"
    return $ toConfig e a i r
    where grab s = get ["devbot", "events", e, s] 

getEvent :: String -> IO (Maybe Event)
getEvent e = do
    c <- getConfig e
    d <- getData e
    return $ parse c d
    where parse Nothing _ = Nothing
          parse _ Nothing = Nothing
          parse (Just c) (Just d) = Just (Event c d)


events :: IO [Maybe Event]
events = do
    es <- keys ["devbot", "events"]
    configs <- mapM getEvent es
    return configs
