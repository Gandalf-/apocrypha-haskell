module Apocrypha.Database
    ( Action(..)
    , Operations
    , action
    , getDB, saveDB
    ) where

import           Data.Aeson

import           Data.List             (intercalate, sort)
import           Data.Maybe            (fromMaybe)
import           System.Directory      (getHomeDirectory)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.Vector           as V


data Action = Action
        { value   :: !Value
        , changed :: !Bool
        , result  :: ![String]
        , top     :: !Object
        , context :: Bool
        }
    deriving (Show, Eq)

type Operations = [String]


action :: Action -> Operations -> Action

-- aliases
action a ("-k" : v) = action a ("--keys" : v)
action a ("-s" : v) = action a ("--set"  : v)
action a ("-d" : v) = action a ("--del"  : v)
action a ("-e" : v) = action a ("--edit" : v)
action a ("-p" : v) = action a ("--pop"  : v)
action a ("-c" : v) = action a ("--context" : v)


-- context
action (Action v c r t _) ("--context" : xs) =
        action (Action v c r t True) xs


-- get
action (Action value _ _ t context) [] =
        Action value False result t context
    where
        result :: [String]
        result =
            if context
                then map ("= " ++) $ pretty value
                else pretty value


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
        value = pretty $ V.head a

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
        Action value c [dump value] t con


-- index
action a@(Action (Object o) changed output t context) (k : xs) =
        if deref
            then dereference a (derefK : xs)
            else Action newBase newChanged result t context
    where
        (Action newValue newChanged newOutput _ _) =
            action (Action childDB changed [] t context) xs

        newBase :: Value
        newBase = Object
                . HM.filter (not . empty)
                . HM.insert key newValue $ o

        childDB :: Value
        childDB  = HM.lookupDefault newEntry key o

        newEntry :: Value
        newEntry = Object $ HM.fromList []

        result  = if context
                      then output ++ map ((k ++ " ") ++) newOutput
                      else output ++ newOutput
        key     = T.pack k
        deref   = head k == '!'
        derefK  = tail k


action (Action db _ _ _ _) _ =
        dbError db "cannot index through non-dict"


dereference :: Action -> Operations -> Action
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

dereference a [] = a


apply :: Operations -> Value -> Action -> Action
apply xs (String s) a = action a (T.unpack s : xs)
apply _ _ a           = a


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


pretty :: Value -> [String]
pretty Null = []
pretty (Array v) = [intercalate "\n" . concatMap pretty . V.toList $ v]
pretty v@(Object o) =
        if HM.null o
            then []
            else result
    where
        result :: [String]
        result = [tail . init $ go (dump v) 0]

        go :: String -> Int -> String
        go [] _        = "\n"
        go ('{': cs) d = "\n" ++ replicate d ' ' ++ go cs (d + 2)
        go ('[': cs) d = "\n" ++ replicate d ' ' ++ go cs (d + 2)

        go (',': cs) d = "\n" ++ replicate (d - 2) ' ' ++ go cs d

        go ('}': cs) d = go cs (d - 2)
        go (']': cs) d = go cs (d - 2)

        go ('"': cs) d = "'" ++ go cs d
        go (':': cs) d = ": " ++ go cs d
        go (c  : cs) d = c : go cs d

pretty (String s) = [T.unpack s]
pretty v = [show v]


-- IO utilities
dbError :: Value -> String -> Action
dbError v msg =
        Action v False ["error: " ++ msg] HM.empty False


getDB :: IO Value
getDB = do
        file <- defaultDB
        fromMaybe Null . decodeStrict . B8.pack <$> readFile file


saveDB :: Value -> IO ()
saveDB value = do
        file <- defaultDB
        B.writeFile file $ encode value


defaultDB = (++ "/.db.json") <$> getHomeDirectory
