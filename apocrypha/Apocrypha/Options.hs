module Apocrypha.Options
    ( Options(..)
    , getOptions
    , usage
    ) where


data Options = Options
        { _enableLog     :: Bool
        , _enableCache   :: Bool
        , _enablePersist :: Bool
        }
    deriving (Show, Eq)


usage :: String
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  --headless     do not log to the console"
        , "  --no-cache     do not using caching"
        , "  --stateless    do not save to disk"
        ]


getOptions :: [String] -> Maybe Options
getOptions args
        | parseError = Nothing
        | otherwise  = Just $ Options enableLog enableCache enablePersist
    where
        enablePersist = NoState `notElem` options
        enableLog   = NoLogging `notElem` options
        enableCache = NoCaching `notElem` options
        parseError  = InvalidOption `elem` options

        options = map parse args


-- | Internal

data Option
        = NoLogging
        | NoCaching
        | NoState
        | InvalidOption
    deriving (Show, Eq)


parse :: String -> Option
parse "--no-cache"  = NoCaching
parse "--headless"  = NoLogging
parse "--stateless" = NoState
parse _             = InvalidOption
