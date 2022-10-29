module Apocrypha.Options where

import           Apocrypha.Internal.Options
import           Network.Socket             (PortNumber)

{-|
    Module      : Apocrypha.Options
    Description : Server command line options
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

class SharedOptions a where
    headless :: a -> Bool
    tcpPort  :: a -> PortNumber

data DatabaseOptions = DatabaseOptions
        { _dbEnableLog     :: Bool
        , _dbTcpPort       :: PortNumber
        , _dbEnableCache   :: Bool
        , _dbEnablePersist :: Bool
        , _dbEnableUnix    :: Bool
        , _dbPath          :: FilePath
        }
    deriving (Show, Eq)

instance SharedOptions DatabaseOptions where
    headless = _dbEnableLog
    tcpPort  = _dbTcpPort

data ProxyOptions = ProxyOptions
        { _proxyEnableLog :: Bool
        , _proxyTcpPort   :: PortNumber
        , _proxyTarget    :: (String, PortNumber)
        }
    deriving (Show, Eq)

instance SharedOptions ProxyOptions where
    headless = _proxyEnableLog
    tcpPort  = _proxyTcpPort

usage :: String
-- ^ help text to display to the user on error or request
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  common options:"
        , "    --headless         do not log to the console"
        , "    --tcp-port port    use an alternate tcp port"
        , ""
        , "  database options:"
        , "    --no-cache         do not using caching"
        , "    --stateless        do not save to disk"
        , "    --no-unix          do not listen on a unix domain socket"
        , "    --database path    use an alternate database"
        , ""
        , "  proxy options:"
        , "    --proxy URI (port) act as a proxy to this server"
        ]


parseOptions :: FilePath -> PortNumber -> [String] -> Maybe (Either DatabaseOptions ProxyOptions)
-- ^ given the default values for database path and port number, parse arguments into options
parseOptions defaultDbPath defaultTCPPort args
        | parseError = Nothing
        | otherwise  = Just $ maybe
            (Left database)
            (Right . proxy)
            proxyTarget
    where
        database :: DatabaseOptions
        database = DatabaseOptions
            enableLog tcpport
            enableCache enablePersist enableUnix dbPath

        proxy :: (String, PortNumber) -> ProxyOptions
        proxy = ProxyOptions
            enableLog tcpport

        options       = parse args
        parseError    = InvalidOption `elem` options

        enablePersist = NoState   `notElem` options
        enableLog     = NoLogging `notElem` options
        enableCache   = NoCaching `notElem` options
        enableUnix    = NoUnix    `notElem` options
        dbPath        = chooseDB options defaultDbPath
        tcpport       = choosePort options defaultTCPPort
        proxyTarget   = parseProxy options defaultTCPPort
