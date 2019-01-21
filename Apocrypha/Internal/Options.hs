module Apocrypha.Internal.Options where

{-|
    Module      : Apocrypha.Internal.Options
    Description : Server command line options
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

data Option
        = NoLogging
        | NoCaching
        | NoState
        | NoUnix
        | OtherDatabase String
        | InvalidOption
    deriving (Show, Eq)


chooseDB :: [Option] -> FilePath -> FilePath
-- ^ look through the provided options for an alternate database, if one
-- isn't found, use the default
chooseDB [] p                  = p
chooseDB (OtherDatabase p:_) _ = p
chooseDB (_:xs) p              = chooseDB xs p


parse :: [String] -> [Option]
parse []                  = []
parse ("--database":x:xs) = OtherDatabase x : parse xs
parse ("--no-cache":xs)   = NoCaching : parse xs
parse ("--no-unix":xs)    = NoUnix : parse xs
parse ("--headless":xs)   = NoLogging : parse xs
parse ("--stateless":xs)  = NoState : parse xs
parse (_:xs)              = InvalidOption : parse xs
