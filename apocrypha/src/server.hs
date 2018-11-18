{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async (async)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as B8
import qualified Data.Text                as T
import           Network
import           System.Exit              (die)
import           System.IO

import           Apocrypha.Database       (Query, getDB, runAction, saveDB)
import           Apocrypha.Protocol       (protoRead, protoSend)
import           Data.Aeson


type WriteReq = MVar Bool
type Database = MVar Value
type ServerApp = ReaderT ThreadData IO

data ThreadData = ThreadData
        { threadHandle :: !Handle
        , userTableMV  :: !Database
        , writeRequest :: !WriteReq
        }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        server <- listenOn $ PortNumber 9999
        putStrLn "Server started"

        db <- getDB :: IO Value
        case db of
            Null -> die "Could not parse database on disk"
            _    -> do
                dbMV <- newMVar db
                wrMV <- newMVar False
                void . async $ runReaderT diskWriter (ThreadData stdout dbMV wrMV)
                withSocketsDo $ clientForker server dbMV wrMV


clientForker :: Socket -> Database -> WriteReq -> IO b
-- ^ listen for clients, fork off workers
clientForker socket db wr = forever $ do
        (h, _, _) <- accept socket
        hSetBuffering h NoBuffering
        void . async $ runReaderT clientLoop (ThreadData h db wr)


diskWriter :: ServerApp ()
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter = forever $ do
        write <- viewWrite >>= readMVarT
        db <- viewDatabase >>= readMVarT

        liftIO $ threadDelay oneSecond
        when write $ liftIO $ saveDB db
    where oneSecond = 1000000


clientLoop :: ServerApp ()
-- ^ read queries from the client, serve them or quit
clientLoop =
        flip catchError (\_ -> return ()) $ do
              query <- getQuery
              case query of
                    Nothing  -> return ()
                    (Just q) -> serve q >> clientLoop


getQuery :: ServerApp (Maybe ByteString)
-- ^ Read a client query from our network handle
getQuery = viewHandle >>= liftIO . protoRead


serve :: ByteString -> ServerApp ()
-- ^ Run a user query through the database, and send them the result.
-- If the database reports that it changed, we set writeRequest.
serve rawQuery = do

        -- start - shared state critical section
        dbMV <- viewDatabase
        db <- takeMVarT dbMV
        let (result, changed, newDB) = runAction db query
        putMVarT dbMV newDB
        -- end   - shared state critical section

        queryLogger changed query
        viewHandle >>= \h -> liftIO . protoSend h . B8.pack $ result

        wrMV <- viewWrite
        wr <- takeMVarT wrMV
        if changed
            then putMVarT wrMV True
            else putMVarT wrMV wr
    where
        query :: Query
        query = filter (not . null)
              . map T.unpack
              . T.split (== '\n')
              . T.pack
              . B8.unpack $ rawQuery


queryLogger :: Bool -> Query -> ServerApp ()
-- ^ write a summary of the query to stdout
queryLogger c query =
        echoLocal . take 80 $ changed ++ unwords query
    where changed = (if c then '~' else ' ') : " "


-- | MVar Utilities

putMVarT :: MVar a -> a -> ReaderT ThreadData IO ()
putMVarT  = (liftIO . ) . putMVar

readMVarT :: MVar a -> ReaderT ThreadData IO a
readMVarT = liftIO . readMVar

takeMVarT :: MVar a -> ReaderT ThreadData IO a
takeMVarT = liftIO . takeMVar


-- | ReaderT Utilities

viewHandle :: ReaderT ThreadData IO Handle
viewHandle   = asks threadHandle

viewDatabase :: ReaderT ThreadData IO Database
viewDatabase = asks userTableMV

viewWrite :: ReaderT ThreadData IO WriteReq
viewWrite    = asks writeRequest

echoLocal :: String -> ReaderT ThreadData IO ()
echoLocal = liftIO . putStrLn
