{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async (async)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString.Char8    (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network
import           System.Exit              (die)
import           System.IO

import           Apocrypha.Cache          (Cache, emptyCache, get, put)
import           Apocrypha.Database       (Query, getDB, runAction, saveDB)
import           Apocrypha.Protocol       (protoRead, protoSend)
import           Data.Aeson


type WriteNeeded = MVar Bool
type Database = MVar Value
type DbCache  = MVar Cache
type ServerApp = ReaderT ThreadData IO

data ThreadData = ThreadData
        { threadHandle :: !Handle
        , database     :: !Database
        , writeNeeded  :: !WriteNeeded
        , cache        :: !DbCache
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
                chMV <- newMVar emptyCache
                void . async $
                    runReaderT diskWriter (ThreadData stdout dbMV wrMV chMV)

                withSocketsDo $
                    clientForker server dbMV wrMV chMV


clientForker :: Socket -> Database -> WriteNeeded -> DbCache -> IO b
-- ^ listen for clients, fork off workers
clientForker socket db wr ch = forever $ do
        (h, _, _) <- accept socket
        hSetBuffering h NoBuffering
        void . async $ runReaderT clientLoop (ThreadData h db wr ch)


diskWriter :: ServerApp ()
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter = forever $ do
        write <- readMVarT =<< viewWrite
        db <- readMVarT =<< viewDatabase

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
-- If the database reports that it changed, we set writeNeeded.
serve rawQuery = do

        ch <- takeMVarT =<< viewCache
        case get ch query of

            -- cache hit
            Just value -> do
                viewCache >>= putMVarT ch

                queryLogger True False query
                viewHandle >>= \h -> liftIO . protoSend h . encodeUtf8 $ value

            -- cache miss
            Nothing -> do

                db <- takeMVarT =<< viewDatabase

                let (result, changed, newDB) = runAction db query
                    newCache = put ch query result

                viewDatabase >>= putMVarT newDB
                viewCache    >>= putMVarT newCache

                viewHandle >>= \h -> liftIO . protoSend h . encodeUtf8 $ result
                queryLogger False changed query

                when changed $ do
                    -- set writeNeeded
                    _ <- takeMVarT =<< viewWrite
                    viewWrite >>= putMVarT True

                    -- clear the cache
                    _ <- takeMVarT =<< viewCache
                    viewCache >>= putMVarT emptyCache

    where
        query :: Query
        query = filter (not . T.null)
              . T.split (== '\n')
              $ decodeUtf8 rawQuery


queryLogger :: Bool -> Bool -> Query -> ServerApp ()
-- ^ write a summary of the query to stdout
queryLogger hit write query =
        echoLocal . T.take 80 $ status `T.append` T.unwords query
    where
        status
            | hit && write = "? "       -- this shouldn't happen
            | hit          = "  "
            | write        = "~ "
            | otherwise    = "* "       -- no hit, no write


-- | MVar Utilities

putMVarT :: a -> MVar a -> ReaderT ThreadData IO ()
putMVarT thing place = liftIO $ putMVar place thing

{-
putMVarT :: MVar a -> a -> ReaderT ThreadData IO ()
putMVarT  = (liftIO . ) . putMVar
-}

readMVarT :: MVar a -> ReaderT ThreadData IO a
readMVarT = liftIO . readMVar

takeMVarT :: MVar a -> ReaderT ThreadData IO a
takeMVarT = liftIO . takeMVar


-- | ReaderT Utilities

viewHandle :: ReaderT ThreadData IO Handle
viewHandle = asks threadHandle

viewDatabase :: ReaderT ThreadData IO Database
viewDatabase = asks database

viewWrite :: ReaderT ThreadData IO WriteNeeded
viewWrite = asks writeNeeded

viewCache :: ReaderT ThreadData IO DbCache
viewCache = asks cache

echoLocal :: Text -> ReaderT ThreadData IO ()
echoLocal = liftIO . putStrLn . T.unpack
