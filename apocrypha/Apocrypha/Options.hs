module Apocrypha.Options
    ( Options(..)
    , getOptions
    , usage
    ) where


data Options = Options
        { _enableLog     :: Bool
        , _enableCache   :: Bool
        , _enablePersist :: Bool
        , _otherDatabase :: String
        }
    deriving (Show, Eq)


usage :: String
usage = unlines
        [ "usage: [OPTION ...]"
        , ""
        , "  --headless        do not log to the console"
        , "  --no-cache        do not using caching"
        , "  --stateless       do not save to disk"
        , "  --database path   use an alternate database"
        ]


getOptions :: FilePath -> [String] -> Maybe Options
getOptions path args
        | parseError = Nothing
        | otherwise  = Just $ Options enableLog enableCache enablePersist database
    where
        enablePersist = NoState `notElem` options
        enableLog     = NoLogging `notElem` options
        enableCache   = NoCaching `notElem` options
        parseError    = InvalidOption `elem` options
        database      = chooseDB options path

        options = parse args


-- | Internal

data Option
        = NoLogging
        | NoCaching
        | NoState
        | OtherDatabase String
        | InvalidOption
    deriving (Show, Eq)


chooseDB :: [Option] -> String -> String
chooseDB [] p = p
chooseDB (OtherDatabase p:_) _ = p
chooseDB (_:xs) p = chooseDB xs p


parse :: [String] -> [Option]
parse [] = []
parse ("--database":x:xs) = OtherDatabase x : parse xs
parse ("--no-cache":xs)   = NoCaching : parse xs
parse ("--headless":xs)   = NoLogging : parse xs
parse ("--stateless":xs)  = NoState : parse xs
parse (_:xs)              = InvalidOption : parse xs
