module Apocrypha.Network2
    ( client, jClient
    , getContext, Context
    , protoSend, protoRead
    ) where

import Network

import Data.Binary (encode, decode)
import Data.List (intercalate)
import GHC.IO.Handle.Types (Handle)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8


type Context = Handle


getContext :: Maybe (String, PortNumber) -> IO Context
getContext Nothing =
        getContext $ Just ("127.0.0.1", 9999)
getContext (Just (host, port)) =
        connectTo host $ PortNumber port


protoSend :: Context -> ByteString -> IO ()
protoSend h = B8.hPut h . protocol


protoRead :: Context -> IO (Maybe ByteString)
protoRead h = do
        rawSize <- B8.hGetSome h 4

        if B8.length rawSize /= 4
            then return Nothing
            else do
                let bytes = B8.append (B8.replicate 4 '\0') rawSize
                    size  = decode (B.fromStrict bytes) :: Int
                result <- B8.hGetSome h size
                return . Just $ result


protocol :: ByteString -> ByteString
protocol message =
        B8.append (len message) message

    where len = B8.drop 4 . B.toStrict . encode . B8.length


client :: Context -> [String] -> IO String
client c query = do
        protoSend c . B8.pack . intercalate "\n" $ query
        result <- protoRead c
        case result of
            Nothing  -> return ""
            (Just r) -> return $ B8.unpack r

jClient :: Context -> [String] -> IO B.ByteString
jClient = undefined
