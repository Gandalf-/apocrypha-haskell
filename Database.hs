module Database where

import Data.Aeson

import Control.Monad (unless)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V


data Action = Action
            { value :: Value
            , changed :: Bool
            , result :: [String]
            }
    deriving (Show, Eq)

type Operations = [String]


runner args = do
    db <- getDB Nothing
    case db of
        Null -> putStrLn "Could not parse database"
        _    -> handle db args


handle :: Value -> Operations -> IO ()
handle database args = do
        saveDB Nothing newDB

        unless (null output)
            (putStrLn $ intercalate "\n" output)
    where
        (Action newDB _ output) = action baseAction args
        baseAction = Action database False []


action :: Action -> Operations -> Action

-- aliases
action a ("-k" : v) = action a $ "--keys" : v
action a ("-s" : v) = action a $ "--set"  : v
action a ("-d" : v) = action a $ "--del"  : v
action a ("-e" : v) = action a $ "--edit" : v
action a ("-p" : v) = action a $ "--pop"  : v


-- get
action (Action value _ output) [] =
        Action value False $ output ++ [pretty value]


-- keys - object
action (Action db@(Object o) _ output) ("--keys" : _) =
        Action db False $ output ++ keys
    where keys = sort . map T.unpack $ HM.keys o

-- keys - bottom
action (Action a _ _) ("--keys" : _) =
        dbError a "cannot retrieve keys for non-dict"


-- assign
action (Action db _ r) ("=" : values) =
        if empty new || db == new
            then Action db  False r
            else Action new True  r
    where new = toValue values


-- set
action (Action v _ output) ("--set" : values) =
        update parsed
    where
        parsed :: Maybe Value
        parsed =
            if null values
                then Nothing
                else decodeStrict . B8.pack . head $ values

        update Nothing  = dbError v "unable to parse JSON"
        update (Just a) =
            if v == a
                then Action v False output
                else Action a True  output


-- del
action (Action _ _ output) ("--del" : _) =
        Action Null True output


-- append - array
action (Action (Array a  ) _ output) ("+" : values) =
        Action (Array new) True output
    where
        new   = a V.++ right
        right = V.fromList . map (String . T.pack) $ values

-- append - object
action (Action db@(Object o) _ output) ("+" : values) =
    -- we allow appending to a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then action emptyAction ("+" : values)
            else dbError db "cannot append to a dictionary"

    where emptyAction = Action emptyArray True output
          emptyArray  = Array . V.fromList $ []

-- append - string
action (Action value@(String _) _ output) ("+" : values) =
        Action (Array new) True output
    where
        new   = left V.++ right
        left  = V.fromList [value]
        right = V.fromList . map (String . T.pack) $ values

-- append - bottom
action (Action a _ _) ("+" : _) =
        dbError a "this type does not support addition"


-- subtract - array
action (Action (Array a) _ output) ("-" : values) =
        if V.length new == 1
            then Action (V.head new) True output
            else Action (Array  new) True output
    where
        new :: V.Vector Value
        new   = V.filter (`notElem` right) a
        right = V.fromList . map (String . T.pack) $ values

-- subtract - string
action (Action value@(String s) c output) ("-" : v) =
        if s `elem` values
            then Action Null  True output
            else Action value c    output
    where
        values = map T.pack v

-- subtract - bottom
action (Action a _ _) ("-" : _) =
        dbError a "this type does not support subtraction"


-- pop - array
action (Action (Array a) _ output) ("--pop" : _) =
        Action (Array . V.tail $ a) True $ output ++ [pretty value]

    where value = V.head a

-- pop - string
action (Action (String s) _ output) ("--pop" : _) =
        Action Null True $ output ++ [T.unpack s]

-- pop - object
action (Action db@(Object o) _ output) ("--pop" : _) =
    -- we allow popping from a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then Action db False output
            else dbError db "cannot pop from a dictionary"


-- pop - bottom
action (Action a _ _) ("--pop" : _) =
        dbError a "this type does not support pop"


-- edit
action (Action value c output) ("--edit" : _) =
        Action value c $ output ++ [dump value]


-- index
action (Action (Object o) c r) (k : xs) =
        Action new newC $ r ++ res
    where
        new     = Object . cleanup . HM.insert key newValue $ o
        cleanup = HM.filter (not . empty)

        (Action newValue newC res) = action (Action childDB c r) xs

        childDB  = HM.lookupDefault newEntry key o
        newEntry = Object $ HM.fromList []
        key      = T.pack k


action (Action db _ _) _ =
        dbError db "cannot index through non-dict"


-- json utilities
dump :: Value -> String
dump = B8.unpack . B.toStrict . encode


toValue :: [String] -> Value
toValue [x] = String . T.pack $ x
toValue xs  = Array . V.fromList . map (String . T.pack) $ xs


empty :: Value -> Bool
empty (Object o) = HM.null o
empty (Array  a) = V.null a
empty (String s) = T.null s
empty Null       = True
empty _          = False

pretty :: Value -> String
pretty Null = ""
pretty (Array v) = intercalate "\n" . map pretty . V.toList $ v
pretty v@(Object _) = result
    where
        result = tail . init $ go (dump v) 0
        go :: String -> Int -> String
        go [] _ = "\n"
        go ('{': cs) d = "\n" ++ replicate d ' ' ++ go cs (d + 2)
        go ('[': cs) d = "\n" ++ replicate d ' ' ++ go cs (d + 2)

        go (',': cs) d = "\n" ++ replicate (d - 2) ' ' ++ go cs d

        go ('}': cs) d = go cs (d - 2)
        go (']': cs) d = go cs (d - 2)

        go ('"': cs) d = "'" ++ go cs d
        go (':': cs) d = ": " ++ go cs d
        go (c  : cs) d = c : go cs d

pretty (String s) = T.unpack s
pretty v = show v


-- IO utilities
dbError :: Value -> String -> Action
dbError v msg = Action v False ["error: " ++ msg]


getDB :: Maybe FilePath -> IO Value
getDB f = fromMaybe Null . decodeStrict . B8.pack <$> readFile file
    where file = fromMaybe defaultDB f


saveDB :: Maybe FilePath -> Value -> IO ()
saveDB f = B.writeFile file . encode
    where file = fromMaybe defaultDB f


defaultDB = "/tmp/db.json"
