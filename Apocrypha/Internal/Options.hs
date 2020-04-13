module Apocrypha.Internal.Options where

import           Data.List.Split (splitOn)
import           Network

{-|
    Module      : Apocrypha.Internal.Options
    Description : Server command line options
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

-- | Types that parse can return
data Option
        = NoLogging
        | NoCaching
        | NoState
        | NoUnix
        | OtherDatabase String
        | OtherTCPPort String
        | Proxy String
        | InvalidOption
    deriving (Show, Eq)


chooseDB :: [Option] -> FilePath -> FilePath
-- ^ look through the provided options for an alternate database, if one
-- isn't found, use the default
chooseDB [] p                  = p
chooseDB (OtherDatabase p:_) _ = p
chooseDB (_:xs) p              = chooseDB xs p

choosePort :: [Option] -> PortNumber -> PortNumber
-- ^ look through the provided options for an alternate tcp port, if one
-- isn't found, use the default
choosePort [] p                 = p
choosePort (OtherTCPPort p:_) _ = read p :: PortNumber
choosePort (_:xs) p             = choosePort xs p

parseProxy :: [Option] -> PortNumber -> Maybe (String, PortNumber)
parseProxy [] _           = Nothing
parseProxy (Proxy uri:_) defaultTCPPort =
    case splitOn ":" uri of
        [host, port] -> Just (host, read port :: PortNumber)
        [host]       -> Just (host, defaultTCPPort)
        _            -> Nothing
parseProxy (_:xs) p       = parseProxy xs p


parse :: [String] -> [Option]
-- ^ attempt to convert string arguments into known options
parse []                  = []
parse ("--database":x:xs) = OtherDatabase x : parse xs
parse ("--tcp-port":x:xs) = OtherTCPPort x : parse xs
parse ("--no-cache":xs)   = NoCaching : parse xs
parse ("--no-unix":xs)    = NoUnix : parse xs
parse ("--headless":xs)   = NoLogging : parse xs
parse ("--stateless":xs)  = NoState : parse xs
parse ("--proxy":uri:xs)  = Proxy uri : parse xs
parse (_:xs)              = InvalidOption : parse xs
