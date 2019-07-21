module Apocrypha.Options
    ( Options(..)
    , getOptions
    , usage
    ) where

import           Apocrypha.Internal.Options
import           Network                    (PortNumber)

{-|
    Module      : Apocrypha.Options
    Description : Server command line options
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

-- | All options that the server understands
data Options = Options
        { _enableLog     :: Bool
        , _enableCache   :: Bool
        , _enablePersist :: Bool
        , _databasePath  :: String
        , _tcpPort       :: PortNumber
        , _enableUnix    :: Bool
        }
    deriving (Show, Eq)


usage :: String
-- ^ help text to display to the user on error or request
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  --headless        do not log to the console"
        , "  --no-cache        do not using caching"
        , "  --stateless       do not save to disk"
        , "  --no-unix         do not listen on a unix domain socket"
        , "  --tcp-port port   use an alternate tcp port"
        , "  --database path   use an alternate database"
        ]


getOptions :: FilePath -> PortNumber -> [String] -> Maybe Options
-- ^ given the default values for database path and port number, parse arguments into options
getOptions defaultDbPath defaultTCPPort args
        | parseError = Nothing
        | otherwise  = Just $
            Options enableLog enableCache enablePersist database tcpport enableUnix
    where
        options       = parse args
        parseError    = InvalidOption `elem` options

        enablePersist = NoState   `notElem` options
        enableLog     = NoLogging `notElem` options
        enableCache   = NoCaching `notElem` options
        enableUnix    = NoUnix    `notElem` options
        database      = chooseDB options defaultDbPath
        tcpport       = choosePort options defaultTCPPort
