module Network.Apocrypha.Database
( Action(..)
, Operations
, action
, getDB, saveDB
) where

import           Data.Aeson

import           Data.List             (intercalate, sort)
import           Data.Maybe            (fromMaybe)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T
import qualified Data.Vector           as V


data Action = Action
        { value   :: Value
        , changed :: Bool
        , result  :: [String]
        }
    deriving (Show, Eq)

type Operations = [String]
type Top = Object


action :: Action -> Operations -> Top -> Action

-- aliases
action a ("-k" : v) t = action a ("--keys" : v) t
action a ("-s" : v) t = action a ("--set"  : v) t
action a ("-d" : v) t = action a ("--del"  : v) t
action a ("-e" : v) t = action a ("--edit" : v) t
action a ("-p" : v) t = action a ("--pop"  : v) t


-- get
action (Action value _ _) [] _ =
        Action value False $ pretty value


-- keys - object
action (Action db@(Object o) _ _) ("--keys" : _) _ =
        Action db False keys
    where keys = sort . map T.unpack $ HM.keys o

-- keys - bottom
action (Action a _ _) ("--keys" : _) _ =
        dbError a "cannot retrieve keys for non-dict"


-- assign
action (Action db _ r) ("=" : values) _ =
        if empty new || db == new
            then Action db  False r
            else Action new True  r
    where new = toValue values


-- set
action (Action v _ output) ("--set" : values) _ =
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
action (Action _ _ output) ("--del" : _) _ =
        Action Null True output


-- append - array
action (Action (Array a  ) _ output) ("+" : values) _ =
        Action (Array new) True output
    where
        new   = a V.++ right
        right = V.fromList . map (String . T.pack) $ values

-- append - object
action (Action db@(Object o) _ output) ("+" : values) t =
    -- we allow appending to a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then action emptyAction ("+" : values) t
            else dbError db "cannot append to a dictionary"

    where emptyAction = Action emptyArray True output
          emptyArray  = Array . V.fromList $ []

-- append - string
action (Action value@(String _) _ output) ("+" : values) _ =
        Action (Array new) True output
    where
        new   = left V.++ right
        left  = V.fromList [value]
        right = V.fromList . map (String . T.pack) $ values

-- append - bottom
action (Action a _ _) ("+" : _) _ =
        dbError a "this type does not support addition"


-- subtract - array
action (Action (Array a) _ output) ("-" : values) _ =
        if V.length new == 1
            then Action (V.head new) True output
            else Action (Array  new) True output
    where
        new :: V.Vector Value
        new = V.filter (`notElem` right) a

        right = V.fromList . map (String . T.pack) $ values

-- subtract - string
action (Action value@(String s) c output) ("-" : v) _ =
        if s `elem` values
            then Action Null  True output
            else Action value c    output
    where
        values :: [T.Text]
        values = map T.pack v

-- subtract - bottom
action (Action a _ _) ("-" : _) _ =
        dbError a "this type does not support subtraction"


-- pop - array
action (Action (Array a) _ _) ("--pop" : _) _ =
        Action (Array $ V.tail a) True value

    where
        value = pretty $ V.head a

-- pop - string
action (Action (String s) _ _) ("--pop" : _) _ =
        Action Null True [T.unpack s]

-- pop - object
action (Action db@(Object o) _ output) ("--pop" : _) _ =
    -- we allow popping from a dictionary only if it's empty,
    -- since that means it's new and hasn't been assigned a value
        if HM.null o
            then Action db False output
            else dbError db "cannot pop from a dictionary"


-- pop - bottom
action (Action a _ _) ("--pop" : _) _ =
        dbError a "this type does not support pop"


-- edit
action (Action value c _) ("--edit" : _) _ =
        Action value c [dump value]


-- index
action a@(Action (Object o) changed output) (k : xs) t =
        if deref
            then dereference a (derefK : xs) t
            else Action newBase newChanged $ output ++ newOutput
    where
        (Action newValue newChanged newOutput) =
            action (Action childDB changed output) xs t

        newBase = Object
                . HM.filter (not . empty)
                . HM.insert key newValue $ o

        childDB  = HM.lookupDefault newEntry key o
        newEntry = Object $ HM.fromList []
        key      = T.pack k

        deref   = head k == '!'
        derefK  = tail k


action (Action db _ _) _ _ =
        dbError db "cannot index through non-dict"


dereference :: Action -> Operations -> Top -> Action
dereference original (k : xs) top =
        if key `elem` HM.keys top
            then case value of
                     -- the dereferenced value is just a string
                     (String s) -> action newBase (T.unpack s : xs) top

                     -- the dereferenced value is an array, we have to
                     -- apply the remaining arguments to each member
                     (Array a)  -> foldl (apply top xs) newBase a
                     _          -> original

            else original

    where
        newBase = Action (Object top) False []
        value = HM.lookupDefault Null key top
        key = T.pack k

dereference a [] _ = a


apply :: Top -> Operations -> Action -> Value -> Action
apply t xs a (String s) = action a (T.unpack s : xs) t
apply _ _ a _           = a


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
dbError v msg = Action v False ["error: " ++ msg]


getDB :: Maybe FilePath -> IO Value
getDB f = fromMaybe Null . decodeStrict . B8.pack <$> readFile file
    where file = fromMaybe defaultDB f


saveDB :: Maybe FilePath -> Value -> IO ()
saveDB f = B.writeFile file . encode
    where file = fromMaybe defaultDB f


defaultDB = "/tmp/db.json"
