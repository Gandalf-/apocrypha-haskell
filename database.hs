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

main :: IO ()
main = getArgs >>= runner

runner args = do
    db <- database
    handle db args

handle :: Value -> [String] -> IO ()
handle Null  _= putStrLn "Could not parse database"
handle database args = do
    let (Action newDB output) = action baseAction args
    _ <- saveDB newDB

    unless (null output)
        (putStrLn $ intercalate "\n" output)

    where baseAction = Action database []


-- get
action :: Action -> [String] -> Action
action (Action db result) [] =
    -- standard query
    Action db $ result ++ [repr]
    where repr = dump db


-- keys
action a ("-k" : _ ) = action a ["--keys"]

action (Action db@(Object o) result) ("--keys" : _) =
    Action db $ result ++ keys
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

-- del
action a ("-d" : _ ) = action a ["--del"]
action (Action db result) ("--del" : _) = Action Null result


-- append
action (Action db@(Array a) result) ("+" : values) =
        Action (Array new) result
    where
        new :: V.Vector Value
        new = a V.++ right

        right :: V.Vector Value
        right = V.fromList . map (String . T.pack) $ values

action (Action db@(Object o) result) ("+" : values) =
    -- we allow appending to a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then action emptyAction ("+" : values)
            else dbError db "cannot not append to a dictionary"

    where emptyAction = Action emptyArray result
          emptyArray = Array . V.fromList $ []

action (Action value result) ("+" : values) =
        Action (Array new) result
    where
        new  = left V.++ right
        left = V.fromList [value]
        right = V.fromList . map (String . T.pack) $ values


-- subtract


-- edit
action a ("-e" : _ ) = action a ["--edit"]

action (Action db r) ("--edit" : _) =
    Action db $ r ++ repr
    where repr = [dump db]


-- index
action (Action a@(Object o) r) (k : xs) =

    Action (Object $
            HM.filter (not . empty) $
            HM.insert key newValue o) $
            r ++ res

    where
        (Action newValue res) = action (Action childDB r) xs

        newEntry = Object $ HM.fromList []
        key      = T.pack k
        childDB  = HM.lookupDefault newEntry key o


action (Action db _) _ = dbError db "cannot index through non-dict"


-- json utilities
dump :: Value -> String
dump = B8.unpack . B.toStrict . encode


toValue :: [String] -> Value
toValue [x] = String . T.pack $ x
toValue xs  = Array . V.fromList . map (String . T.pack) $ xs


empty :: Value -> Bool
empty (Object o) = HM.null o
empty (Array  a) = V.null  a
empty (String s) = T.null s
empty Null       = True
empty _          = False


-- IO utilities
dbError :: Value -> String -> Action
dbError v msg = Action v ["error: " ++ msg]

database :: IO Value
database = do
    content <- readFile databaseFile
    return $ fromMaybe Null (decodeStrict (B8.pack content) :: Maybe Value)

    where databaseFile = "/tmp/db.json"

saveDB :: Value -> IO ()
saveDB v = B.writeFile "/tmp/db.json" $ encode v
