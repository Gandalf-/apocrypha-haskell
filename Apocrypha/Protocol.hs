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
    , Context, getContext, defaultContext, unixSocketPath
    , protoSend, protoRead, protocol
    , Query
    ) where

import           Control.Exception     (SomeException, try)
import           Data.Binary           (decode, encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import           Data.List             (intercalate)
import           GHC.IO.Handle.Types   (Handle)
import           Network
import           System.Directory      (getTemporaryDirectory)
import           System.FilePath.Posix ((</>))


type Context = Maybe Handle
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


client :: Context -> Query -> IO (Maybe String)
-- ^ Make a remote query using the provided context
client Nothing _ = pure Nothing
client (Just c) query = do
        protoSend c . BS.pack $ intercalate "\n" query
        fmap BS.unpack <$> protoRead c


jClient :: Context -> Query -> IO (Maybe BL.ByteString)
-- ^ Make a remote query using the provided context, no processing is done
-- with the result - it's handed back exactly as it's read off the socket
jClient Nothing _ = pure Nothing

jClient (Just c) query = do
        protoSend c . BS.pack $ intercalate "\n" query
        fmap BL.fromStrict <$> protoRead c


defaultContext :: IO Context
-- ^ Try to conect to the local database, prefer unix domain socket
defaultContext = do
        unixPath <- unixSocketPath

        let unixSock = getContext $ Right (local, unixPath)
            tcpSock  = getContext $ Left  (local, 9999)

        s <- unixSock
        case s of
            (Just _) -> pure s
            _        -> tcpSock
    where
        local = "127.0.0.1"

getContext :: Either HostTCP HostUnix -> IO Context
-- ^ Attempt to connect to a TCP or Unix host
#ifdef mingw32_HOST_OS
getContext (Right _) = do
        pure Nothing
#else
getContext (Right (host, path)) = do
        result <- try (connectTo host $ UnixSocket path
                      ) :: HandleOrException
        pure $ eitherToMaybe result
#endif

getContext (Left (host, port)) = do
        result <- try (connectTo host $ PortNumber port
                      ) :: HandleOrException
        pure $ eitherToMaybe result


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

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a
