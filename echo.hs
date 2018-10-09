module Main where

import Network.Socket
import Data.List (intercalate)

import Data.Aeson
import Database
import Apocrypha.Network

import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Char8 as B8


main :: IO ()
main = withSocketsDo $ do
    db <- getDB Nothing
    case db of
        Null -> putStrLn "Could not parse database on disk"
        _    -> do
          listener <- socket AF_INET Stream defaultProtocol
          setSocketOption listener ReuseAddr 1
          bind listener $ SockAddrInet port iNADDR_ANY
          listen listener 2
          runServer db listener

    where port = 8888


runServer :: Value -> Socket -> IO ()
runServer db listener = do
    -- get a client
    (sock,_) <- accept listener

    -- handle all their requests
    db <- serve db sock

    -- get a new client
    runServer db listener


serve :: Value -> Socket -> IO Value
serve db sock = do

    -- read the query
    query <- protoRead sock

    case query of
      Nothing  -> return db
      (Just q) -> do
          let msg = filter (not . null) . map B8.unpack . B8.split '\n' $ q
          start <- getTime
          (reply, newDB) <- worker db msg

          protoSend sock $ B8.pack reply
          end <- getTime

          putStrLn $ show (end - start) ++ " " ++ show msg
          serve newDB sock


worker :: Value -> Operations -> IO (String, Value)
worker database query =
        -- saveDB Nothing newDB
        return (intercalate "\n" output, newDB)
    where
        (Action newDB output) = action baseAction query
        baseAction = Action database []

-- getTime :: IO Integer
getTime = getPOSIXTime
