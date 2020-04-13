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
    , Context(..), getContext, defaultContext, getServerlessContext, getMemoryContext, getHybridContext
    , unixSocketPath, defaultTCPPort
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


data Context =
          NoConnection
        | NetworkConnection Handle
        | Serverless FilePath
        | CachingServerless FilePath (TVar Value)
        | MemoryDB (TVar Value)
-- ^ Potential connection to an Apocrypha client or server

type Query = [String]
-- ^ Elements of a query

type HostTCP  = (String, PortNumber)
-- ^ Description of a TCP remote host

type HostUnix = (String, String)
-- ^ Description of a Unix Domain socket remote host


unixSocketPath :: IO String
-- ^ Newer versions of Windows support AF_UNIX, so place nice with paths
unixSocketPath = (</> "apocrypha.sock") <$> getTemporaryDirectory

defaultTCPPort :: PortNumber
-- ^ default TCP IP port to listen on for servers, or to connect to as a client
defaultTCPPort = 9999


serverlessQuery :: FilePath ->  Query -> IO T.Text
serverlessQuery path query = do
        db <- getDB path
        let (result, changed, newDB) = runAction db $ map T.pack query

        when changed $
            saveDB path newDB

        pure result

memoryQuery :: TVar Value -> Query -> IO T.Text
memoryQuery d query =
        atomically $ do
            db <- readTVar d
            let (result, _, newDB) = runAction db $ map T.pack query
            writeTVar d newDB
            pure result

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


client :: Context -> Query -> IO (Maybe String)
-- ^ Make a remote query using the provided context
client NoConnection _ = pure Nothing

client (NetworkConnection c) query = do
        protoSend c . BS.pack $ intercalate "\n" query
        fmap BS.unpack <$> protoRead c

client (Serverless path) query =
        Just . T.unpack <$> serverlessQuery path query

client (MemoryDB db) query =
        Just . T.unpack <$> memoryQuery db query

client (CachingServerless path db) query =
        Just . T.unpack <$> hybridQuery path db query


jClient :: Context -> Query -> IO (Maybe BL.ByteString)
-- ^ Make a remote query using the provided context, no processing is done
-- with the result - it's handed back exactly as it's read off the socket
jClient NoConnection _ = pure Nothing

jClient (NetworkConnection c) query = do
        protoSend c . BS.pack $ intercalate "\n" query
        fmap BL.fromStrict <$> protoRead c

jClient (Serverless path) query =
        Just . BL.fromStrict . encodeUtf8 <$> serverlessQuery path query

jClient (MemoryDB db) query =
        Just . BL.fromStrict . encodeUtf8 <$> memoryQuery db query

jClient (CachingServerless path db) query =
        Just . BL.fromStrict . encodeUtf8 <$> hybridQuery path db query


defaultContext :: IO Context
-- ^ Try to conect to the local database, prefer unix domain socket
defaultContext = do
        unixPath <- unixSocketPath

        let unixSock = getContext $ Right (local, unixPath)
            tcpSock  = getContext $ Left  (local, defaultTCPPort)

        s <- unixSock
        case s of
            (NetworkConnection _) -> pure s
            _                     -> tcpSock
    where
        local = "127.0.0.1"


getHybridContext :: FilePath -> IO Context
getHybridContext path =
        CachingServerless path <$> (newTVarIO =<< getDB path)


getServerlessContext :: FilePath -> IO Context
-- ^ create a serverless context, where each query reads or writes from
-- self managed database in a file
getServerlessContext = pure . Serverless


getMemoryContext :: IO Context
getMemoryContext = do
        d <- newTVarIO . Object $ HM.fromList []
        pure $ MemoryDB d


getContext :: Either HostTCP HostUnix -> IO Context
-- ^ Attempt to connect to a TCP or Unix host
#ifdef mingw32_HOST_OS
getContext (Right _) = do
        pure NoConnection
#else
getContext (Right (host, path)) = do
        result <- try (connectTo host $ UnixSocket path
                      ) :: HandleOrException
        pure $ eitherToNetCon result
#endif

getContext (Left (host, port)) = do
        result <- try (connectTo host $ PortNumber port
                      ) :: HandleOrException
        pure $ eitherToNetCon result


eitherToNetCon :: Either a Handle -> Context
eitherToNetCon (Left _)  = NoConnection
eitherToNetCon (Right h) = NetworkConnection h


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
