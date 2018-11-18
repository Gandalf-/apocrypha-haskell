module Apocrypha.Database
    ( Action(..)
    , Query
    , Context(..)
    , action
    , getDB, saveDB
    , runAction
    ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)

import           Data.List                (intercalate, sort)
import           Data.Maybe               (fromMaybe)
import           System.Directory         (getHomeDirectory)

import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as B
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import qualified Data.Vector              as V


data Action = Action
        { _value   :: !Value
        , _changed :: !Bool
        , _result  :: ![String]
        , _top     :: !Object
        , _context :: !Context
        }
    deriving (Show, Eq)

type Query = [String]

data Context = Context
        { _enabled :: !Bool
        , _members :: ![String]
        }
    deriving (Show, Eq)


-- | Action - core logic of the database
--
-- queries traverse down levels of the database until they hit an action,
-- the output and level is then mutated and passsed back up to the top

action :: Action -> Query -> Action

-- aliases
action a ("-k" : v) = action a ("--keys" : v)
action a ("-s" : v) = action a ("--set"  : v)
action a ("-d" : v) = action a ("--del"  : v)
action a ("-e" : v) = action a ("--edit" : v)
action a ("-p" : v) = action a ("--pop"  : v)
action a ("-c" : v) = action a ("--context" : v)


-- context
action (Action v c r t (Context _ m)) ("--context" : xs) =
        action (Action v c r t (Context True m)) xs


-- get
action (Action value _ _ t context) [] =
        Action value False result t context
    where
        result :: [String]
        result = pretty context value


-- keys - object
action (Action db@(Object o) _ _ t c) ("--keys" : _) =
        Action db False keys t c
    where keys = sort . map T.unpack $ HM.keys o

-- keys - bottom
action (Action a _ _ _ _) ("--keys" : _) =
        dbError a "cannot retrieve keys for non-dict"


-- assign
action (Action db _ r t c) ("=" : values) =
        if empty new || db == new
            then Action db  False r t c
            else Action new True  r t c
    where
        new :: Value
        new = toValue values


-- set
action (Action v _ output t c) ("--set" : values) =
        update parsed
    where
        parsed :: Maybe Value
        parsed =
            if null values
                then Nothing
                else decodeStrict . B8.pack . head $ values

        update :: Maybe Value -> Action
        update Nothing  = dbError v "unable to parse JSON"
        update (Just a) =
            if v == a
                then Action v False output t c
                else Action a True  output t c


-- del
action (Action _ _ output t c) ("--del" : _) =
        Action Null True output t c


-- append - array
action (Action (Array a  ) _ output t c) ("+" : values) =
        Action (Array new) True output t c
    where
        new :: V.Vector Value
        new   = a V.++ right

        right :: V.Vector Value
        right = V.fromList . map (String . T.pack) $ values

-- append - object
action (Action db@(Object o) _ output t c) ("+" : values) =
        -- we allow appending to a dictionary only if it's empty,
        -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then action emptyAction ("+" : values)
            else dbError db "cannot append to a dictionary"

    where
        emptyAction = Action emptyArray True output t c
        emptyArray  = Array . V.fromList $ []

-- append - string
action (Action value@(String _) _ output t c) ("+" : values) =
        Action (Array new) True output t c
    where
        new   = left V.++ right
        left  = V.fromList [value]
        right = V.fromList . map (String . T.pack) $ values

-- append - bottom
action (Action a _ _ _ _) ("+" : _) =
        dbError a "this type does not support addition"


-- subtract - array
action (Action (Array a) _ output t c) ("-" : values) =
        if V.length new == 1
            then Action (V.head new) True output t c
            else Action (Array  new) True output t c
    where
        new :: V.Vector Value
        new   = V.filter (`notElem` right) a
        right = V.fromList . map (String . T.pack) $ values

-- subtract - string
action (Action value@(String s) changed output t c) ("-" : v) =
        if s `elem` values
            then Action Null  True    output t c
            else Action value changed output t c
    where
        values :: [T.Text]
        values = map T.pack v

-- subtract - bottom
action (Action a _ _ _ _) ("-" : _) =
        dbError a "this type does not support subtraction"


-- pop - array
action (Action (Array a) _ _ t c) ("--pop" : _) =
        Action (Array $ V.tail a) True value t c
    where
        value = pretty c $ V.head a

-- pop - string
action (Action (String s) _ _ t c) ("--pop" : _) =
        Action Null True [T.unpack s] t c

-- pop - object
action (Action db@(Object o) _ output t c) ("--pop" : _) =
        -- we allow popping from a dictionary only if it's empty,
        -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then Action db False output t c
            else dbError db "cannot pop from a dictionary"


-- pop - bottom
action (Action a _ _ _ _) ("--pop" : _) =
        dbError a "this type does not support pop"


-- edit
action (Action value c _ t con) ("--edit" : _) =
        Action value c [showValue value] t con


-- index
action a@(Action (Object o) changed previousOutput top context) (k : xs) =
        if deref
            then dereference a (derefK : xs)
            else Action newBase newChanged output top context
    where
        (Action newValue newChanged newOutput _ _) =
            action (Action nextDB changed [] top nextContext) xs

        newBase :: Value
        newBase = Object
                . HM.filter (not . empty)
                . HM.insert key newValue $ o

        nextDB :: Value
        nextDB  = HM.lookupDefault (Object $ HM.fromList []) key o

        nextContext :: Context
        nextContext = Context (_enabled context) (_members context ++ [k])

        output  = previousOutput ++ newOutput
        key     = T.pack k
        deref   = head k == '!'
        derefK  = tail k


-- absolute bottom
action (Action db _ _ _ _) _ =
        dbError db "cannot index through non-dict"


-- | Misc Utilities

dereference :: Action -> Query -> Action
dereference original@(Action _ _ _ top c)  (k : xs) =
        if key `elem` HM.keys top
            then case value of
                     -- the dereferenced value is just a string
                     (String s) -> action newBase (T.unpack s : xs)

                     -- the dereferenced value is an array, we have to
                     -- apply the remaining arguments to each member
                     (Array a)  -> foldr (apply xs) newBase a
                     _          -> original

            else original
    where
        newBase :: Action
        newBase = Action (Object top) False [] top c

        value = HM.lookupDefault Null key top
        key = T.pack k

        apply :: Query -> Value -> Action -> Action
        apply os (String s) a = action a (T.unpack s : os)
        apply _ _ a           = a

dereference a [] = a

toValue :: [String] -> Value
toValue [x] = String . T.pack $ x
toValue xs  = Array . V.fromList . map (String . T.pack) $ xs

empty :: Value -> Bool
-- ^ determine if a polymorphic value is empty
empty (Object o) = HM.null o
empty (Array  a) = V.null a
empty (String s) = T.null s
empty Null       = True
empty _          = False


-- | Presentation
showValue :: Value -> String
showValue = B8.unpack . B.toStrict . encodePretty

pretty :: Context -> Value -> [String]
pretty _ Null = []
pretty c (Array v) =
        [intercalate "\n" . concatMap (pretty c) . V.toList $ v]

pretty _ v@(Object o) =
        if HM.null o
            then []
            else [showValue v]

pretty (Context True m) (String s) = addContext m $ T.unpack s
pretty (Context _ _)    (String s) = [T.unpack s]

pretty (Context True m) v = addContext m $ show v
pretty (Context _ _)    v = [show v]

addContext :: [String] -> String -> [String]
-- ^ create the context explanation for a value
-- context is a list of keys that we had to traverse to get to the value
addContext context value =
        [intercalate " = " $ safeInit context ++ [value]]
    where
        safeInit [] = []
        safeInit xs = init xs


-- | IO utilities
runAction :: Value -> Query -> (String, Bool, Value)
-- ^ run the query through the database and produce the artifacts
runAction db query =
        (result, changed, newDB)
    where
        (Action newDB changed output _ _) = action baseAction query

        baseAction :: Action
        baseAction =
            case db of
                (Object o) -> Action db False [] o (Context False [])
                _          -> error "database top level is not a map"

        result :: String
        result = intercalate "\n" output ++ "\n"


dbError :: Value -> String -> Action
-- ^ create an error out of this level to pass back up, do not modify the
-- value, do not report changes
dbError v msg =
        Action v False ["error: " ++ msg] HM.empty (Context False [])

getDB :: IO Value
getDB = do
        file <- defaultDB
        fromMaybe Null . decodeStrict . B8.pack <$> readFile file

saveDB :: Value -> IO ()
saveDB v = do
        file <- defaultDB
        B.writeFile file $ encode v

defaultDB :: IO String
defaultDB = (++ "/.db.json") <$> getHomeDirectory
