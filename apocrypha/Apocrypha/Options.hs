module Apocrypha.Options
    ( Options(..)
    , getOptions
    , usage
    ) where


data Options = Options
        { _enableLog   :: Bool
        , _enableCache :: Bool
        }


data Option
        = NoLogging
        | NoCaching
        | InvalidOption
    deriving (Eq)


usage :: String
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  --headless     do not log to the console"
        , "  --no-cache     do not using caching"
        ]


getOptions :: [String] -> Maybe Options
getOptions args
        | parseError = Nothing
        | otherwise  = Just $ Options enableLog enableCache
    where
        enableLog   = NoLogging `notElem` options
        enableCache = NoCaching `notElem` options
        parseError  = InvalidOption `elem` options

        options = map parse args


parse :: String -> Option
parse "--no-cache" = NoCaching
parse "--headless" = NoLogging
parse _            = InvalidOption
