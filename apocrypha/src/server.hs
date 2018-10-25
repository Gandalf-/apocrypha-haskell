{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async (async)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Char8    (ByteString)
import           Data.List                (intercalate)
import           Network
import           System.Directory         (getHomeDirectory)
import           System.IO

import           Apocrypha.Database
import           Apocrypha.Protocol
import           Data.Aeson

import qualified Data.ByteString.Char8    as B8
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T


type WriteReq = MVar Bool
type Database = MVar Value
type ServerApp = ReaderT ThreadData IO

data ThreadData = ThreadData
        { threadHandle :: Handle
        , userTableMV  :: Database
        , writeRequest :: WriteReq
        }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        server <- listenOn $ PortNumber 9999
        putStrLn "Server started"
        dbPath <- (++ "/.db.json") <$> getHomeDirectory
        db <- getDB $ Just dbPath
        case db of
            Null -> putStrLn "Could not parse database on disk"
            _    -> do
                dbMV <- newMVar db
                wrMV <- newMVar False
                void . async $ runReaderT diskWriter (ThreadData stdout dbMV wrMV)
                withSocketsDo $ clientForker server dbMV wrMV


clientForker :: Socket -> Database -> WriteReq -> IO b
-- ^ listen for clients, fork off workers
clientForker socket db wr = do
        (h, _, _) <- accept socket
        hSetBuffering h NoBuffering
        void . async $ runReaderT clientLoop (ThreadData h db wr)
        clientForker socket db wr


diskWriter :: ServerApp ()
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter = forever $ do
        write <- viewWrite >>= readMVarT
        db <- viewDatabase >>= readMVarT

        liftIO $ threadDelay oneSecond
        when write $ liftIO . saveDB Nothing $ db
    where oneSecond = 1000000


clientLoop :: ServerApp ()
-- ^ read queries from the client, serve them or quit
clientLoop =
        flip catchError (\_ -> return ()) $ do
          query <- getQuery
          case query of
            Nothing  -> return ()
            (Just q) -> do serve q
                           clientLoop


getQuery :: ServerApp (Maybe ByteString)
-- ^ Read a client query from our network handle
getQuery = viewHandle >>= liftIO . protoRead


serve :: ByteString -> ServerApp ()
-- ^ Run a user query through the database, and send them the result.
-- If the database reports that it changed, we set writeRequest.
serve t = do
        dbMV <- viewDatabase
        db <- takeMVarT dbMV
        let (result, changed, newDB) = runAction db query
        putMVarT dbMV newDB

        queryLogger changed query
        viewHandle >>= \h -> liftIO . protoSend h . B8.pack $ result

        wrMV <- viewWrite
        wr <- takeMVarT wrMV

        if changed
            then putMVarT wrMV True
            else putMVarT wrMV wr
    where
        query = filter (not . null)
              . map T.unpack
              . T.split (== '\n') $ text

        text = T.pack . B8.unpack $ t


runAction :: Value -> Operations -> (String, Bool, Value)
-- ^ run the query through the database and produce the artifacts
runAction db query =
        (result, changed, newDB)
    where
        (Action newDB changed output) =
            case db of
                (Object o) -> action baseAction query o
                _          -> action baseAction query $ HM.fromList []

        baseAction = Action db False []
        result = intercalate "\n" output ++ "\n"


queryLogger :: Bool -> [String] -> ServerApp ()
-- ^ write a summary of the query to stdout
queryLogger c query =
        echoLocal . take 80 $ changed ++ unwords query
    where changed = (if c then '~' else ' ') : " "


putMVarT  = (liftIO . ) . putMVar
readMVarT = liftIO . readMVar
takeMVarT = liftIO . takeMVar

viewHandle   = asks threadHandle
viewDatabase = asks userTableMV
viewWrite    = asks writeRequest

echoLocal = liftIO . putStrLn
