module Apocrypha.Protocol
    ( client, jClient
    , Context, getContext, defaultContext
    , protoSend, protoRead
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

-- | Clients
client :: Context -> Query -> IO (Maybe String)
-- ^ make a remote query using the provided context
client Nothing _ = return Nothing
client (Just c) query = do
        protoSend c . B8.pack . intercalate "\n" $ query
        result <- protoRead c
        case result of
            Nothing -> return Nothing
            Just s  -> return . Just . B8.unpack $ s


jClient :: Context -> Query -> IO (Maybe B.ByteString)
-- ^ make a remote query using the provided context, no processing is done
-- with the result - it's handed back exactly as it's read off the socket
jClient Nothing _ = return Nothing

jClient (Just c) query = do
        protoSend c . B8.pack . intercalate "\n" $ query
        Just . B.fromStrict . fromMaybe B8.empty <$> protoRead c


-- | Contexts
defaultContext :: IO Context
defaultContext = getContext Nothing

getContext :: Maybe (String, PortNumber) -> IO Context
getContext Nothing =
        getContext $ Just ("127.0.0.1", 9999)

getContext (Just (host, port)) = do
        result <- try (connectTo host $ PortNumber port
                      ) :: IO (Either SomeException Handle)
        case result of
            Left _  -> return Nothing
            Right h -> return $ Just h


-- | Protocol
protoSend :: Handle -> ByteString -> IO ()
protoSend h = B8.hPut h . protocol

protoRead :: Handle -> IO (Maybe ByteString)
protoRead handle = do
        rawSize <- B8.hGetSome handle 4

        if B8.length rawSize /= 4
            then return Nothing
            else do
                let bytes = B8.append (B8.replicate 4 '\0') rawSize
                    size  = decode (B.fromStrict bytes) :: Int
                result <- reader handle B8.empty size
                return $ Just result

reader :: Handle -> ByteString -> Int -> IO ByteString
reader handle previous bytesRemaining
    | bytesRemaining <= 0 = return previous
    | otherwise           = do
         this <- B8.hGetSome handle bytesRemaining
         next <- reader handle this (bytesRemaining - B8.length this)
         return $ B8.append previous next

protocol :: ByteString -> ByteString
-- ^ The Apocrypha protocol is simple - send 4 bytes to represent the length
-- of the message, then the message
protocol message =
        B8.append (len message) message
    where
        len :: ByteString -> ByteString
        len = B8.drop 4 . B.toStrict . encode . B8.length
