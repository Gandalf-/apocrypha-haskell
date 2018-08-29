module Apocrypha.Network where

import Control.Exception (SomeException, try)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as B

import Data.Binary (encode)
import Data.List (intersperse)



protocol :: String -> B8.ByteString
protocol message = 
    B8.append len msg
    where len = htonl' $ length message
          msg = B8.pack message
          htonl' = B8.drop 4 . B.toStrict . encode


unprotocol :: B8.ByteString -> Maybe String
unprotocol bytes = clean result
  where result = B8.unpack $ B8.drop 4 bytes
        clean [] = Nothing
        clean xs = Just $ init xs


query :: Socket -> [String] -> IO (Maybe String)
query sock message = do
    _ <- send sock $ protocol msg
    buffer <- recv sock bufferSize
    return $ unprotocol buffer
    where msg = concat $ intersperse "\n" message
          bufferSize = 1024 ^ 2


type ExceptOrIO = IO (Either SomeException ())

client :: [String] -> IO (Maybe String)
client message = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    canConnect <- try (connect sock (addrAddress serverAddr)) :: ExceptOrIO
    case canConnect of
      Left _ -> return Nothing
      Right _ -> do
        reply <- query sock message
        close sock
        return reply
    where host = "127.0.0.1"
          port = 9999
