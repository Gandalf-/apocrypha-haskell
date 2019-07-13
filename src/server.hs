{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.Async (async)
import           Control.Monad.Except
import           Data.Aeson               (Value (..))
import           Data.ByteString.Char8    (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network
import           System.Environment       (getArgs)
import           System.Exit              (die)
import           System.IO
#ifndef mingw32_HOST_OS
import           System.Directory         (doesFileExist, removeFile)
#endif

import           Apocrypha.Database       (Query, defaultDB, getDB, runAction,
                                           saveDB)
import           Apocrypha.Internal.Cache (Cache, emptyCache, get, put)
import           Apocrypha.Options
#ifndef mingw32_HOST_OS
import           Apocrypha.Protocol       (defaultTCPPort, protoRead, protoSend,
                                           unixSocketPath)
#else
import           Apocrypha.Protocol       (defaultTCPPort, protoRead, protoSend)
#endif


type WriteNeeded = TVar Bool
type Database = TVar Value
type DbCache  = TVar Cache
type ClientCount = TVar Integer

data ThreadData = ThreadData
        { _threadHandle :: !Handle
        , _database     :: !Database
        , _writeNeeded  :: !WriteNeeded
        , _clientCount  :: !ClientCount
        , _cache        :: !DbCache
        , _options      :: !Options
        }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        defaultPath <- defaultDB
        arguments <- getOptions defaultPath defaultTCPPort <$> getArgs

        case arguments of
            Nothing -> die usage

            Just options -> do
                db <- (getDB $ _databasePath options) :: IO Value
                case db of
                    Null -> die "Could not parse database on disk"
                    _    -> startup db options
    where
        startup :: Value -> Options -> IO ()
        startup db options = withSocketsDo $ do
            tcpSocket  <- listenOn $ PortNumber $ _tcpPort options

            putStrLn "Server started"
            dbMV <- newTVarIO db          -- in memory database
            chMV <- newTVarIO emptyCache  -- in memory cache
            wrMV <- newTVarIO False       -- do we have writes to flush to disk?
            ccMV <- newTVarIO 0           -- running client total

            let env = ThreadData stdout dbMV wrMV ccMV chMV options
            when (_enablePersist options) $
                persistThread env

#ifndef mingw32_HOST_OS
            unixSocket <- getUnixSocket
            -- listen on both sockets
            when (_enableUnix options) $
                void . async $
                    clientForker unixSocket dbMV wrMV ccMV chMV options
#endif

            clientForker tcpSocket dbMV wrMV ccMV chMV options

        persistThread :: ThreadData -> IO ()
        persistThread = void . async . diskWriter

#ifndef mingw32_HOST_OS
        getUnixSocket :: IO Socket
        getUnixSocket = do
            unixPath <- unixSocketPath
            exists <- doesFileExist unixPath
            when exists $
                removeFile unixPath
            listenOn $ UnixSocket unixPath
#endif


clientForker :: Socket
    -> Database
    -> WriteNeeded
    -> ClientCount
    -> DbCache
    -> Options
    -> IO b
-- ^ listen for clients, fork off workers
clientForker socket d w n c o = forever $ do
        atomically $ do
            count <- readTVar n
            when (count > maxClients) retry

        -- accept a new client connection, set buffering, increment client total
        (h, _, _) <- accept socket
        hSetBuffering h NoBuffering
        atomically $ modifyTVar n (+ 1)

        -- start client loop to handle queries
        void . async . clientLoop $ ThreadData h d w n c o
    where
        maxClients   = 500


diskWriter :: ThreadData -> IO ()
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter (ThreadData _ d w _ _ o) = forever $ do

        threadDelay oneSecond
        needWrite <- readTVarIO w

        when needWrite $ do
            db <- readTVarIO d
            saveDB path db
    where
        oneSecond = 1000000
        path = _databasePath o


clientLoop :: ThreadData -> IO ()
-- ^ read queries from the client, serve them or quit
clientLoop td = do
        query <- protoRead client
        case query of
            (Just q) -> do
                -- run the query through the database, reply to the client, wait for
                -- another query
                serve td q
                clientLoop td

            Nothing  -> do
                -- close the connection, decrement total clients
                hClose client
                atomically $
                    modifyTVar (_clientCount td) (\x -> x - 1)
    where
        client = _threadHandle td


serve :: ThreadData -> ByteString -> IO ()
-- ^ Run a user query through the database, and send them the result.
-- If the database reports that it changed, we set writeNeeded.
serve (ThreadData client d w cc c o) rawQuery = do

        cache <- readTVarIO c
        case get (_enableCache o) cache query of

            -- cache hit
            Just value -> do
                reply client value
                when (_enableLog o) $
                    logToConsole cc cacheHit noChange query

            -- cache miss, or disabled
            Nothing -> do
                value <- atomically $ do
                    db <- readTVar d

                    let (result, changed, newDB) = runAction db query
                        newCache = put cache query result

                    writeTVar d newDB

                    if changed
                        -- set writeNeeded, clear the cache
                        then writeTVar w True >> writeTVar c emptyCache

                        -- no change, just update the cache
                        else writeTVar c newCache

                    pure result

                reply client value
                when (_enableLog o) $
                    logToConsole cc cacheMiss noChange query

    where
        query :: Query
        query = filter (not . T.null) . T.split (== '\n')
              $ decodeUtf8 rawQuery

        reply :: Handle -> Text -> IO ()
        reply h value = protoSend h $ encodeUtf8 value

        (cacheHit, cacheMiss, noChange) = (True, False, False)


logToConsole :: ClientCount -> Bool -> Bool -> Query -> IO ()
-- ^ write a summary of the query to stdout
logToConsole cc hit write query = do
        count <- readTVarIO cc
        putStrLn . T.unpack . T.take 80 $
            clients count <> status <> T.unwords query
    where
        status
            | hit && write = "? "       -- this shouldn't happen
            | hit          = "  "
            | write        = "~ "
            | otherwise    = "* "       -- no hit, no write

        clients count
            | count < 10  = " "
            | count < 50  = "."
            | count < 100 = "o"
            | count < 250 = "O"
            | count < 450 = "0"
            | otherwise   = "!"
