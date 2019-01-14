{-# LANGUAGE OverloadedStrings #-}

module Apocrypha.Internal.Database where

import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as B
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Vector              as V
import           System.Directory         (doesFileExist, getHomeDirectory,
                                           renameFile)
import           System.FilePath.Posix    ((</>))


type Query = [Text]

data Action = Action
        { _value   :: !Value
        , _changed :: !Bool
        , _result  :: ![Text]
        , _top     :: !Object
        , _context :: !Context
        }
    deriving (Show, Eq)

data Context = Context
        { _enabled :: !Bool
        , _members :: ![Text]
        }
    deriving (Show, Eq)


-- | Presentation
showValue :: Value -> Text
showValue = decodeUtf8 . B.toStrict . encoder
    where
        encoder = P.encodePretty' config
        config = P.Config (P.Spaces 4) P.compare P.Generic False


pretty :: Context -> Value -> [Text]
pretty _ Null = []
pretty c (Array v) =
        [T.intercalate "\n" . concatMap (pretty c) . V.toList $ v]

pretty _ v@(Object o)
        | HM.null o = []
        | otherwise = [showValue v]

pretty (Context True m) (String s) = addContext m s
pretty (Context _ _)    (String s) = [s]

pretty (Context True m) v = addContext m $ showValue v
pretty (Context _ _)    v = [showValue v]


addContext :: [Text] -> Text -> [Text]
-- ^ create the context explanation for a value
-- context is a list of keys that we had to traverse to get to the value
addContext context value =
        [T.intercalate " = " $ safeInit context ++ [value]]
    where
        safeInit [] = []
        safeInit xs = init xs


baseAction :: Value -> Action
baseAction db =
    case db of
        (Object o) -> Action db False [] o (Context False [])
        _          -> error "database top level is not a map"


-- | IO utilities
dbError :: Value -> Text -> Action
-- ^ create an error out of this level to pass back up, do not modify the
-- value, do not report changes
dbError v msg =
        Action v False ["error: " <> msg] HM.empty (Context False [])


getDB :: FilePath -> IO Value
getDB path = do
        exists <- doesFileExist path
        if exists
          then fromMaybe Null . decodeStrict . B8.pack <$> readFile path
          else do
               writeFile path "{}"
               getDB path


saveDB :: FilePath -> Value -> IO ()
-- ^ atomic write + move into place
saveDB path v = do
        B8.writeFile tmpFile $ encodeUtf8 $ showValue v
        renameFile tmpFile path
    where
        tmpFile = path <> ".tmp"


defaultDB :: IO String
defaultDB = (</> ".db.json") <$> getHomeDirectory
