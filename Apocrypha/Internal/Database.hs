{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
    Module      : Apocrypha.Internal.Database
    Description : Core database logic
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

module Apocrypha.Internal.Database where

import           Codec.Compression.Zlib   (compress, decompress)
import           Control.Exception        (SomeException, evaluate, try)
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.Aeson.KeyMap        as HM
import qualified Data.ByteString.Char8    as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Maybe               (fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Vector              as V
import           System.Directory
import           System.FileLock
import           System.FilePath.Posix    ((</>))
import           System.IO                (IOMode (..), withFile)


-- | items to parse into a database query
type Query = [Text]

-- | dynamic state for each level in the database
data Action = Action
        { _value   :: !Value    -- ^ The content of the database at this level
        , _changed :: !Bool     -- ^ Has the content changed?
        , _result  :: ![Text]   -- ^ The result of the computation, may be empty
        , _top     :: !Object   -- ^ The top level of the database
        , _context :: !Context  -- ^ Additional context to apply to the result
        }
    deriving (Show, Eq)

-- | additional information that may be added to the result
data Context = Context
        { _enabled :: !Bool
        , _members :: ![Text]
        }
    deriving (Show, Eq)


-- | Presentation
showValue :: Value -> Text
showValue = decodeUtf8 . BL.toStrict . encoder
    where
        encoder = P.encodePretty' config
        config = P.Config (P.Spaces 4) P.compare P.Generic False


pretty :: Context -> Value -> [Text]
-- ^ database content aware printer. returned as an array so newlines may be
-- interspersed if needed between distinct items
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
-- ^ create the top level action to begin a query with
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
-- ^ attempt to read the database
-- if it doesn't exist, create an empty db and read that
-- if it's compressed, decompress, otherwise read it plain
--
-- has to do strict IO to avoid file locking issues on Windows
getDB path = do
        exists <- doesFileExist path
        if exists
          then do
            -- take a shared lock to read the database
            lock <- lockPath
            withFileLock lock Shared
                (\ _ -> fromMaybe Null . decodeStrict <$> safeRead)

          else do
            -- save an empty database, then read that
            saveDB path emptyDB
            getDB path
    where
        safeRead :: IO BS.ByteString
        safeRead = do
            content <- withFile path ReadMode BS.hGetContents
            result  <- compressRead content

            case result of
                -- unable to decompress the file, try to use it in raw form
                Left _                    -> pure content

                -- we were able to decompress the file, use that
                Right decompressedContent -> pure decompressedContent

        compressRead :: BS.ByteString -> IO (Either SomeException BS.ByteString)
        compressRead b =
            try (evaluate . BL.toStrict . decompress $ BL.fromStrict b)


saveDB :: FilePath -> Value -> IO ()
-- ^ atomic write + move into place
saveDB path v = do
        lock <- lockPath
        withFileLock lock Exclusive $
            const saveDatabase
    where
        saveDatabase :: IO ()
#ifdef mingw32_HOST_OS
        -- windows rename -> overwrite behavior is buggy, just overwrite
        saveDatabase =
            BS.writeFile path $ prepare v
#else
        -- otherwise, do the right thing and atomic rename in place
        saveDatabase = do
            let tmpFile = path <> ".tmp"
            BS.writeFile tmpFile $ prepare v
            renameFile tmpFile path
#endif
        prepare :: Value -> BS.ByteString
        prepare = BL.toStrict . compress . encode


defaultDB :: IO String
-- ^ the default database location, $HOME/.db.json
defaultDB = (</> ".db.json") <$> getHomeDirectory


emptyDB :: Value
-- ^ the smallest valid database
emptyDB = Object $ HM.fromList []


lockPath :: IO FilePath
-- ^ all reads and writes share a lock. this isn't ideal if there are many
-- separate databases on the system, but is better than cluttering up the
-- filesystem with <path>.lock files
lockPath = (</> "apocrypha-read-write.lock") <$> getTemporaryDirectory
