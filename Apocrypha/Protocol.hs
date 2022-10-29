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
    , Context, getContext, defaultContext, contextToHandle
    , ServerType(..), unixSocketPath, defaultTCPPort
    , protoSend, protoRead, protocol
    , Query
    ) where

import           Apocrypha.Database          (getDB, runAction, saveDB)

import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException, try)
import           Control.Monad               (when)
import           Control.Monad.STM           (atomically)
import           Data.Aeson                  hiding (decode, encode)
import qualified Data.Aeson.KeyMap           as HM
import           Data.Binary                 (decode, encode)
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as L
import           Data.List                   (intercalate)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Network.Socket
import           System.Directory            (getTemporaryDirectory)
import           System.FilePath.Posix       ((</>))
import           System.IO

type Query = [String]

data Context =
      ContextNone
    | ContextNetwork Handle
    | ContextServerless FilePath
    | ContextServerLessCaching FilePath (TVar Value)
    | ContextMemory (TVar Value)

data ServerType =
      ServerTcp (String, PortNumber)
    | ServerUnix FilePath
    | ServerMemory
    | Serverless FilePath
    | ServerlessCaching FilePath

--  ____        _     _ _
-- |  _ \ _   _| |__ | (_) ___
-- | |_) | | | | '_ \| | |/ __|
-- |  __/| |_| | |_) | | | (__
-- |_|    \__,_|_.__/|_|_|\___|

unixSocketPath :: IO FilePath
unixSocketPath = (</> "apocrypha.sock") <$> getTemporaryDirectory

defaultTCPPort :: PortNumber
defaultTCPPort = 9999

client :: Context -> Query -> IO (Maybe String)
-- ^ Make a remote query using the provided context
client ContextNone _ = pure Nothing

client (ContextNetwork c) query = do
        protoSend c . C.pack $ intercalate "\n" query
        fmap C.unpack <$> protoRead c

client context query = Just . T.unpack <$> execute context query

jClient :: Context -> Query -> IO (Maybe L.ByteString)
-- ^ Make a remote query using the provided context, no processing is done
-- with the result - it's handed back exactly as it's read off the socket
jClient ContextNone _ = pure Nothing

jClient (ContextNetwork c) query = do
        protoSend c . C.pack $ intercalate "\n" query
        fmap L.fromStrict <$> protoRead c

jClient context query =
        Just . L.fromStrict . encodeUtf8 <$> execute context query

--  _____                     _   _
-- | ____|_  _____  ___ _   _| |_(_) ___  _ __
-- |  _| \ \/ / _ \/ __| | | | __| |/ _ \| '_ \
-- | |___ >  <  __/ (__| |_| | |_| | (_) | | | |
-- |_____/_/\_\___|\___|\__,_|\__|_|\___/|_| |_|

execute :: Context -> Query -> IO T.Text
execute (ContextMemory d) query =
    atomically $ do
        db <- readTVar d
        let (result, _, newDB) = runAction db $ map T.pack query
        writeTVar d newDB
        pure result

execute (ContextServerless path) query = do
    db <- getDB path
    let (result, changed, newDB) = runAction db $ map T.pack query
    when changed $ saveDB path newDB
    pure result

execute (ContextServerLessCaching path d) query = do
    (result, changed) <- atomically $ do
        db <- readTVar d
        let (result, changed, newDB) = runAction db $ map T.pack query
        writeTVar d newDB
        pure (result, changed)
    when changed $ readTVarIO d >>= saveDB path
    pure result

execute _ _ = undefined

--   ____            _            _
--  / ___|___  _ __ | |_ _____  _| |_ ___
-- | |   / _ \| '_ \| __/ _ \ \/ / __/ __|
-- | |__| (_) | | | | ||  __/>  <| |_\__ \
--  \____\___/|_| |_|\__\___/_/\_\\__|___/

contextToHandle :: Context -> Maybe Handle
contextToHandle (ContextNetwork h) = Just h
contextToHandle _                  = Nothing

defaultContext :: IO Context
-- ^ Try to conect to the local database, prefer unix domain socket
defaultContext = do
        unixPath <- unixSocketPath
        let unixSock = getContext $ ServerUnix unixPath
            tcpSock  = getContext $ ServerTcp ("127.0.0.1", defaultTCPPort)
        s <- unixSock
        case s of
            (ContextNetwork _) -> pure s
            _                  -> tcpSock

getContext :: ServerType -> IO Context
getContext (ServerUnix path) = do
#ifdef mingw32_HOST_OS
        pure NoConnection
#else
        sock <- socket AF_UNIX Stream defaultProtocol
        socketToContext sock $ SockAddrUnix path
#endif

getContext (ServerTcp (host, port)) = do
        sock <- socket AF_INET Stream defaultProtocol
        addr:_ <- getAddrInfo Nothing (Just host) (Just $ show port)
        socketToContext sock (addrAddress addr)

getContext ServerMemory = do
        d <- newTVarIO . Object $ HM.fromList []
        pure $ ContextMemory d

getContext (ServerlessCaching path) =
        ContextServerLessCaching path <$> (newTVarIO =<< getDB path)

getContext (Serverless path) = pure (ContextServerless path)

--  ____            _                  _
-- |  _ \ _ __ ___ | |_ ___   ___ ___ | |
-- | |_) | '__/ _ \| __/ _ \ / __/ _ \| |
-- |  __/| | | (_) | || (_) | (_| (_) | |
-- |_|   |_|  \___/ \__\___/ \___\___/|_|

protoSend :: Handle -> C.ByteString -> IO ()
-- ^ Encode and write a bytestring to a handle
protoSend h = C.hPut h . protocol

protoRead :: Handle -> IO (Maybe C.ByteString)
-- ^ This is a blocking call. if the writer says there are more bytes than
-- they actually send, this will wait forever
protoRead handle = do
        rawSize <- C.hGetSome handle 4

        if C.length rawSize /= 4
            then pure Nothing
            else do
                let bytes = C.replicate 4 '\0' <> rawSize
                    size  = decode (L.fromStrict bytes) :: Int
                result <- reader handle C.empty size
                pure $ Just result

protocol :: C.ByteString -> C.ByteString
-- ^ The Apocrypha protocol is simple - send 4 bytes to represent the length
-- of the message, then the message.
-- This means the maximum message size is 2 ** 32 bytes ~ 4.2GB
protocol message =
        len message <> message
    where
        len :: C.ByteString -> C.ByteString
        len = C.drop 4 . L.toStrict . encode . C.length

--  __  __ _
-- |  \/  (_)___  ___
-- | |\/| | / __|/ __|
-- | |  | | \__ \ (__
-- |_|  |_|_|___/\___|

type HandleOrException = IO (Either SomeException Handle)

reader :: Handle -> C.ByteString -> Int -> IO C.ByteString
reader handle previous bytesRemaining
        | bytesRemaining <= 0 = pure previous
        | otherwise           = do
             this <- C.hGetSome handle bytesRemaining
             next <- reader handle this (bytesRemaining - C.length this)
             pure $ previous <> next

socketToContext :: Socket -> SockAddr -> IO Context
socketToContext sock addr = do
        result <- try (
                connect sock addr >> socketToHandle sock ReadWriteMode
            ) :: HandleOrException
        pure $ case result of
            (Left _)  -> ContextNone
            (Right h) -> ContextNetwork h
