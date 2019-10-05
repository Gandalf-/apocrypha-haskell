{-# LANGUAGE CPP #-}

{-|
    Module      : Apocrypha.Protocol
    Description : Protocol primitives
    License     : MIT
    copyright   : 2018, Austin
    Maintainer  : austin@anardil.net
    Stability   : experimental
    Portability : POSIX
-}

module Apocrypha.Protocol
    ( client, jClient
    , Context, connect, connectPath, connectNet
    , unixSocketPath, defaultTCPPort
    , TcpConnection, Serverless, CachingServerless, MemoryDB, UnixConnection
    , protoSend, protoRead, protocol
    , Query
    ) where

import           Apocrypha.Database          (getDB, runAction, saveDB)

import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException, try)
import           Control.Monad               (when)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  hiding (decode, encode)
import           Data.Binary                 (decode, encode)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BL
import qualified Data.HashMap.Strict         as HM
import           Data.List                   (intercalate)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           GHC.IO.Handle.Types         (Handle)
import           Network
import           System.Directory            (getTemporaryDirectory)
import           System.FilePath.Posix       ((</>))


newtype UnixConnection = UnixConnection Handle
newtype TcpConnection  = TcpConnection Handle

newtype Serverless = Serverless FilePath
data CachingServerless = CachingServerless FilePath (TVar Value)
newtype MemoryDB = MemoryDB (TVar Value)

type Query = [String]
-- ^ Elements of a query


unixSocketPath :: IO String
-- ^ Newer versions of Windows support AF_UNIX, so place nice with paths
unixSocketPath = (</> "apocrypha.sock") <$> getTemporaryDirectory

defaultTCPPort :: PortNumber
-- ^ default TCP IP port to listen on for servers, or to connect to as a client
defaultTCPPort = 9999


class Context a where
        client  :: a -> Query -> IO (Maybe String)
        jClient :: a -> Query -> IO (Maybe BL.ByteString)


instance (Context a) => Context (Maybe a) where
        client Nothing  _ = pure Nothing
        client (Just c) q = client c q

        jClient Nothing  _ = pure Nothing
        jClient (Just c) q = jClient c q

-- | Generic handle based instance, used by TcpConnection and UnixConnection
instance Context Handle where
        client c query = do
                protoSend c . BS.pack $ intercalate "\n" query
                fmap BS.unpack <$> protoRead c

        jClient c query = do
                protoSend c . BS.pack $ intercalate "\n" query
                fmap BL.fromStrict <$> protoRead c


instance Context TcpConnection where
        client  t = client (getHandle t)
        jClient t = jClient (getHandle t)


-- | MemoryDB clients
instance Context MemoryDB where
        client (MemoryDB db) query =
                Just . T.unpack <$> memoryQuery db query

        jClient (MemoryDB db) query =
                Just . BL.fromStrict . encodeUtf8 <$> memoryQuery db query

memoryQuery :: TVar Value -> Query -> IO T.Text
memoryQuery d query =
        atomically $ do
            db <- readTVar d
            let (result, _, newDB) = runAction db $ map T.pack query
            writeTVar d newDB
            pure result


-- | Serverless clients
instance Context Serverless where
        client (Serverless path) query =
                Just . T.unpack <$> serverlessQuery path query

        jClient (Serverless path) query =
                Just . BL.fromStrict . encodeUtf8 <$> serverlessQuery path query

serverlessQuery :: FilePath ->  Query -> IO T.Text
serverlessQuery path query = do
        db <- getDB path
        let (result, changed, newDB) = runAction db $ map T.pack query

        when changed $
            saveDB path newDB

        pure result


-- CachingServerless clients
instance Context CachingServerless where
        client (CachingServerless path db) query =
                Just . T.unpack <$> hybridQuery path db query

        jClient (CachingServerless path db) query =
                Just . BL.fromStrict . encodeUtf8 <$> hybridQuery path db query

hybridQuery :: FilePath -> TVar Value -> Query -> IO T.Text
-- ^ always read and write to the memory database first, and if something changed, write
-- it out to disk too. this is safe if only a single application is accessing the
-- persisted database
hybridQuery path d query = do
        (result, changed) <- atomically $ do
            db <- readTVar d
            let (result, changed, newDB) = runAction db $ map T.pack query
            writeTVar d newDB
            pure (result, changed)

        when changed $
            readTVarIO d >>= saveDB path

        pure result


-- | File based contexts
class (Context a) => FileConnection a where
        connectPath :: FilePath -> IO a

instance FileConnection Serverless where
        connectPath = pure . Serverless

instance FileConnection CachingServerless where
        connectPath path =
                CachingServerless path <$> (newTVarIO =<< getDB path)

-- | Memory based contexts
class (Context a) => Connection a where
        connect :: IO a

instance Connection MemoryDB where
        connect = do
                d <- newTVarIO . Object $ HM.fromList []
                pure $ MemoryDB d

-- | Network based contexts
class (Context a) => NetConnection a where
        connectNet :: IO (Maybe a)
        getHandle  :: a -> Handle

instance NetConnection TcpConnection where
        connectNet = do
                result <- try (connectTo host $ PortNumber defaultTCPPort
                        ) :: HandleOrException
                case result of
                    (Left _)  -> pure Nothing
                    (Right h) -> pure $ Just $ TcpConnection h
            where
                host = "127.0.0.1"

        getHandle (TcpConnection h) = h


#ifndef mingw32_HOST_OS
-- Unix instances are only implemented when we're not on Windows

instance Context UnixConnection where
        client  t = client (getHandle t)
        jClient t = jClient (getHandle t)

instance NetConnection UnixConnection where
        connectNet = do
                result <- try (connectTo host $ UnixSocket unixSocketPath
                        ) :: HandleOrException
                case result of
                    (Left _)  -> pure Nothing
                    (Right h) -> pure $ Just $ UnixConnection h
            where
                host = "127.0.0.1"

        getHandle (UnixConnection h) = h
#endif



protoSend :: Handle -> BS.ByteString -> IO ()
-- ^ Encode and write a bytestring to a handle
protoSend h = BS.hPut h . protocol


protoRead :: Handle -> IO (Maybe BS.ByteString)
-- ^ This is a blocking call. if the writer says there are more bytes than
-- they actually send, this will wait forever
protoRead handle = do
        rawSize <- BS.hGetSome handle 4

        if BS.length rawSize /= 4
            then pure Nothing
            else do
                let bytes = BS.replicate 4 '\0' <> rawSize
                    size  = decode (BL.fromStrict bytes) :: Int
                result <- reader handle BS.empty size
                pure $ Just result


protocol :: BS.ByteString -> BS.ByteString
-- ^ The Apocrypha protocol is simple - send 4 bytes to represent the length
-- of the message, then the message.
-- This means the maximum message size is 2 ** 32 bytes ~ 4.2GB
protocol message =
        len message <> message
    where
        len :: BS.ByteString -> BS.ByteString
        len = BS.drop 4 . BL.toStrict . encode . BS.length


-- helpers

type HandleOrException = IO (Either SomeException Handle)

reader :: Handle -> BS.ByteString -> Int -> IO BS.ByteString
reader handle previous bytesRemaining
        | bytesRemaining <= 0 = pure previous
        | otherwise           = do
             this <- BS.hGetSome handle bytesRemaining
             next <- reader handle this (bytesRemaining - BS.length this)
             pure $ previous <> next
