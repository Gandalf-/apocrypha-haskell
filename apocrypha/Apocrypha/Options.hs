module Apocrypha.Options
    ( Options(..)
    , getOptions
    , usage
    ) where

import           Apocrypha.Internal.Options

data Options = Options
        { _enableLog     :: Bool
        , _enableCache   :: Bool
        , _enablePersist :: Bool
        , _databasePath  :: String
        , _enableUnix    :: Bool
        }
    deriving (Show, Eq)


usage :: String
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  --headless        do not log to the console"
        , "  --no-cache        do not using caching"
        , "  --stateless       do not save to disk"
        , "  --no-unix         do not listen on a unix domain socket"
        , "  --database path   use an alternate database"
        ]


getOptions :: FilePath -> [String] -> Maybe Options
getOptions defaultDbPath args
        | parseError = Nothing
        | otherwise  = Just $
            Options enableLog enableCache enablePersist database enableUnix
    where
        options       = parse args
        parseError    = InvalidOption `elem` options

        enablePersist = NoState   `notElem` options
        enableLog     = NoLogging `notElem` options
        enableCache   = NoCaching `notElem` options
        enableUnix    = NoUnix    `notElem` options
        database      = chooseDB options defaultDbPath
