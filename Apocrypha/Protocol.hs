module Apocrypha.Protocol
    ( client, jClient
    , Context, getContext, defaultContext, unixSocketPath
    , protoSend, protoRead, protocol
    , Query
    ) where

import           Network

import           Control.Exception     (SomeException, try)
import           Data.Binary           (decode, encode)
import           Data.List             (intercalate)
import           Data.Maybe            (fromMaybe)
import           GHC.IO.Handle.Types   (Handle)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as B


type Context = Maybe Handle
type Query = [String]

type HostTCP  = (String, PortNumber)
type HostUnix = (String, String)
type HandleOrException = IO (Either SomeException Handle)

unixSocketPath :: String
unixSocketPath = "/tmp/apocrypha.sock"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a


-- | Clients
client :: Context -> Query -> IO (Maybe String)
-- ^ make a remote query using the provided context
client Nothing _ = pure Nothing
client (Just c) query = do
        protoSend c . B8.pack $ intercalate "\n" query
        result <- protoRead c
        pure $ case result of
            Nothing -> Nothing
            Just s  -> Just . B8.unpack $ s


jClient :: Context -> Query -> IO (Maybe B.ByteString)
-- ^ make a remote query using the provided context, no processing is done
-- with the result - it's handed back exactly as it's read off the socket
jClient Nothing _ = pure Nothing

jClient (Just c) query = do
        protoSend c . B8.pack $ intercalate "\n" query
        Just . B.fromStrict . fromMaybe B8.empty <$> protoRead c


-- | Contexts
defaultContext :: IO Context
-- ^ try to conect to the local database, prefer unix socket
defaultContext = do
        s <- unixSock
        case s of
            (Just _) -> pure s
            _        -> tcpSock
    where
        local = "127.0.0.1"
        unixSock = getContext $ Right (local, unixSocketPath)
        tcpSock  = getContext $ Left  (local, 9999)

getContext :: Either HostTCP HostUnix -> IO Context
getContext (Right (host, path)) = do
        result <- try (connectTo host $ UnixSocket path
                      ) :: HandleOrException
        pure $ eitherToMaybe result

getContext (Left (host, port)) = do
        result <- try (connectTo host $ PortNumber port
                      ) :: HandleOrException
        pure $ eitherToMaybe result


-- | Protocol
protoSend :: Handle -> ByteString -> IO ()
protoSend h = B8.hPut h . protocol

protoRead :: Handle -> IO (Maybe ByteString)
-- ^ this is a blocking call. if the writer says there are more bytes than
-- they actually send, this will wait forever
protoRead handle = do
        rawSize <- B8.hGetSome handle 4

        if B8.length rawSize /= 4
            then pure Nothing
            else do
                let bytes = B8.replicate 4 '\0' <> rawSize
                    size  = decode (B.fromStrict bytes) :: Int
                result <- reader handle B8.empty size
                pure $ Just result

reader :: Handle -> ByteString -> Int -> IO ByteString
reader handle previous bytesRemaining
        | bytesRemaining <= 0 = pure previous
        | otherwise           = do
             this <- B8.hGetSome handle bytesRemaining
             next <- reader handle this (bytesRemaining - B8.length this)
             pure $ previous <> next

protocol :: ByteString -> ByteString
-- ^ The Apocrypha protocol is simple - send 4 bytes to represent the length
-- of the message, then the message.
-- This means the maximum message size is 2 ** 32 bytes ~ 4.2GB
protocol message =
        len message <> message
    where
        len :: ByteString -> ByteString
        len = B8.drop 4 . B.toStrict . encode . B8.length
