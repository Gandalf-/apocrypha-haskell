module Main where

import Control.Monad
import Network.Socket
import Data.List (intercalate)

import Data.Aeson
import Database
import Apocrypha.Network

import qualified Data.ByteString.Char8 as B8


main :: IO ()
main = withSocketsDo $ do
    -- port <- toEnum . read . head <$> getArgs
    let port = 8888
    newSocket <- socket AF_INET Stream defaultProtocol
    setSocketOption newSocket ReuseAddr 1
    bind newSocket $ SockAddrInet port iNADDR_ANY
    listen newSocket 2
    runServer newSocket


runServer :: Socket -> IO()
runServer s = forever $ do
  (sock,_) <- accept s
  serve sock


serve :: Socket -> IO ()
serve sock = forever $ do
    msg   <- filter (not . null) . map B8.unpack . B8.split '\n'
             <$> protoRead sock
    print msg

    reply <- B8.pack <$> goop msg
    protoSend sock reply

goop :: [String] -> IO String
goop query = do
    db <- getDB Nothing
    case db of
        Null -> return ""
        _    -> worker db query


worker :: Value -> Operations -> IO String
worker database query = do
        saveDB Nothing newDB
        return . intercalate "\n" $ output
    where
        (Action newDB output) = action baseAction query
        baseAction = Action database []
