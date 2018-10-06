module Main where

import Data.Aeson

import Control.Monad (unless)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V


data Action = Action Value [String]
    deriving (Show, Eq)

type Operations = [String]


main :: IO ()
main = getArgs >>= runner

runner args = do
    db <- getDB Nothing
    case db of
        Null -> putStrLn "Could not parse database"
        _    -> handle db args


handle :: Value -> Operations -> IO ()
handle database args = do
    let (Action newDB output) = action baseAction args
    _ <- saveDB Nothing newDB

    unless (null output)
        (putStrLn $ intercalate "\n" output)

    where baseAction = Action database []


action :: Action -> Operations -> Action

-- aliases
action a ("-k" : v) = action a $ "--keys" : v
action a ("-s" : v) = action a $ "--set"  : v
action a ("-d" : v) = action a $ "--del"  : v
action a ("-e" : v) = action a $ "--edit" : v


-- get
action (Action value output) [] =
        Action value $ output ++ [dump value]


-- keys
action (Action db@(Object o) output) ("--keys" : _) =
        Action db $ output ++ keys
    where keys = sort . map T.unpack $ HM.keys o

action (Action a _) ("--keys" : _) =
    dbError a "cannot retrieve keys for non-dict"


-- assign
action (Action db r) ("=" : values) =
        if empty new
            then Action db r
            else Action new r
    where new = toValue values


-- set
action (Action v _) ["--set"] = dbError v "unable to parse JSON"

action (Action v output) ("--set" : values) =
        update parsed
    where
        parsed :: Maybe Value
        parsed = decodeStrict . B8.pack . head $ values

        update Nothing  = dbError v "unable to parse JSON"
        update (Just a) = Action a output


-- del
action (Action _ output) ("--del" : _) =
        Action Null output


-- append
--   array
action (Action (Array a  ) output) ("+" : values) =
        Action (Array new) output
    where
        new   = a V.++ right
        right = V.fromList . map (String . T.pack) $ values

--   object
action (Action db@(Object o) output) ("+" : values) =
    -- we allow appending to a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then action emptyAction ("+" : values)
            else dbError db "cannot not append to a dictionary"

    where emptyAction = Action emptyArray output
          emptyArray  = Array . V.fromList $ []

--   string
action (Action value@(String _) output) ("+" : values) =
        Action (Array new) output
    where
        new   = left V.++ right
        left  = V.fromList [value]
        right = V.fromList . map (String . T.pack) $ values

--   bottom
action (Action a _) ("+" : _) =
        dbError a "this type does not support addition"


-- subtract
--   array
action (Action (Array a  ) output) ("-" : values) =
        if V.length new == 1
            then Action (V.head new) output
            else Action (Array  new) output
    where
        new :: V.Vector Value
        new   = V.filter (`notElem` right) a
        right = V.fromList . map (String . T.pack) $ values

--   string
action (Action value@(String s) output) ("-" : v) =
        if s `elem` values
            then Action Null  output
            else Action value output
    where
        values = map T.pack v

--   bottom
action (Action a _) ("-" : _) =
        dbError a "this type does not support subtraction"


-- edit
action (Action value output) ("--edit" : _) =
        Action value $ output ++ [dump value]


-- index
action (Action (Object o) r) (k : xs) =
        Action new $ r ++ res
    where
        new     = Object . cleanup . HM.insert key newValue $ o
        cleanup = HM.filter (not . empty)

        (Action newValue res) = action (Action childDB r) xs

        childDB  = HM.lookupDefault newEntry key o
        newEntry = Object $ HM.fromList []
        key      = T.pack k


action (Action db _) _ = dbError db "cannot index through non-dict"


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


-- IO utilities
dbError :: Value -> String -> Action
dbError v msg = Action v ["error: " ++ msg]


getDB :: Maybe FilePath -> IO Value
getDB f = fromMaybe Null . decodeStrict . B8.pack <$> readFile file
    where file = fromMaybe defaultDB f


saveDB :: Maybe FilePath -> Value -> IO ()
saveDB f = B.writeFile file . encode
    where file = fromMaybe defaultDB f


defaultDB = "/tmp/db.json"
