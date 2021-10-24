{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Apocrypha.Server.Plugin.Hasher where

import           Control.Exception
import           Data.Aeson
import           Data.Scientific
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format
import           System.Directory (getModificationTime)
import           Data.Text                   (Text)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Text.Printf
import qualified Data.HashMap.Strict         as HM

import qualified Crypto.Hash.SHA1            as Crypto

import           Apocrypha.Internal.Database (Query)
import           Apocrypha.Server.Plugin

{-
Hasher caches hashes of files for given paths
-}

data Entry = Entry
    { _stamp :: UTCTime
    , _hash  :: Text
    }
    deriving (Show, Eq)

data Hasher = Hasher

instance Plugin Hasher where
    namespace _ = "hasher"
    execute   _ = hasher

hasher :: Value -> Query -> IO (Text, Bool, Value)
hasher db (path:ps) = do
        (t1, c1, newDb) <- hash db path
        (t2, c2, outDb) <- hasher newDb ps
        pure
            ( T.intercalate "\n" [t1, t2]
            , c1 || c2
            , outDb
            )
hasher db [] = pure ("", False, db)

-- | Business logic

hash :: Value -> Text -> IO (Text, Bool, Value)
hash db path =
    case search db path of
        (Just e) -> cacheHit e
        Nothing  -> cacheMiss
    where
        cacheHit :: Entry -> IO (Text, Bool, Value)
        cacheHit e = do
            t <- getModTime path
            case t of
                (Just t') -> checkTime e t'
                Nothing   -> pure failure

        checkTime :: Entry -> UTCTime -> IO (Text, Bool, Value)
        checkTime (Entry stamp value) current
            | stamp == current = pure (value, False, db)
            | otherwise        = cacheMiss

        cacheMiss :: IO (Text, Bool, Value)
        cacheMiss = do
            fs <- getHash path
            case fs of
                (Just (time, v)) ->
                    pure (v, True, update db path (Entry time v))

                Nothing ->
                    pure failure

        failure :: (Text, Bool, Value)
        failure = ("", False, db)


-- | Cache Interaction

entryToValue :: Entry -> Value
entryToValue (Entry s h) =
        Object $ HM.fromList [("s", stamp), ("h", String h)]
    where
        stamp = Number $ flip scientific 0 $ utcToEpoch s

valueToEntry :: Value -> Maybe Entry
valueToEntry (Object o) = do
    sj <- HM.lookup "s" o
    vj <- HM.lookup "h" o

    s <- case sj of
        (Number n) ->
            epochToUtc . toInteger <$>
            (toBoundedInteger n :: Maybe Int)
        _ -> Nothing

    v <- case vj of
        (String t) -> Just t
        _ -> Nothing

    pure $ Entry s v

valueToEntry _ = Nothing

update :: Value -> Text -> Entry -> Value
update (Object o) path entry =
    -- already have some entries, add our new one
    Object $ HM.insert path (entryToValue entry) o
update _ path entry =
    -- no existing entries or they're junk, overwrite
    Object $ HM.fromList [(path, entryToValue entry)]

search :: Value -> Text -> Maybe Entry
-- try to find the hash in the database
search (Object o) path =
        valueToEntry $ HM.lookupDefault Null path o
search _ _ = Nothing


-- | Hashing

getHash :: Text -> IO (Maybe (UTCTime, Text))
-- actually try to hash the file
getHash tpath = handle
        (\ (_ :: IOException) -> pure Nothing) $ do
            v <- toHexString . Crypto.hashlazy <$> BL.readFile path
            t <- getModificationTime path
            pure $ Just (t, v)
    where
        path = T.unpack tpath

toHexString :: BS.ByteString -> Text
toHexString = BS.foldr (\b -> (<>) (T.pack $ printf "%02x" b)) ""


-- | Time

utcToEpoch :: UTCTime -> Integer
utcToEpoch = read . formatTime defaultTimeLocale "%s"

epochToUtc :: Integer -> UTCTime
epochToUtc = posixSecondsToUTCTime . fromIntegral

getModTime :: Text -> IO (Maybe UTCTime)
-- has this file been modified since this time? have to do the same rounding
-- that the cache does otherwise the values will never match
getModTime tpath = handle
        (\ (_ :: IOException) -> pure Nothing) $
        Just . simplify <$> getModificationTime path
    where
        simplify = epochToUtc . utcToEpoch
        path = T.unpack tpath
