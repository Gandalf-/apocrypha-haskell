{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async    (async)
import           Control.Concurrent.STM.TVar
import           Control.Monad               (join)
import           Control.Monad.Except
import           Control.Monad.STM
import           Data.Aeson                  (Value (..))
import           Data.ByteString.Char8       (ByteString)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Network
import           System.Environment          (getArgs)
import           System.Exit                 (die)
import           System.IO
import           System.Timeout              (timeout)
#ifndef mingw32_HOST_OS
import           System.Directory            (doesFileExist, removeFile)
#endif

import           Apocrypha.Database          (Query, defaultDB, getDB,
                                              runAction, saveDB)
import           Apocrypha.Internal.Cache    (Cache, cacheGet, cachePut,
                                              emptyCache)
import           Apocrypha.Options
#ifndef mingw32_HOST_OS
import           Apocrypha.Protocol          (Context (..), defaultTCPPort,
                                              getContext, protoRead, protoSend,
                                              unixSocketPath)
#else
import           Apocrypha.Protocol          (Context (..), defaultTCPPort,
                                              getContext, protoRead, protoSend)
#endif


type WriteNeeded = TVar Bool
type Database = TVar Value
type DbCache  = TVar Cache
type ClientCount = TVar Integer

data ThreadData =
    ServerData
        { _database    :: !Database
        , _writeNeeded :: !WriteNeeded
        , _clientCount :: !ClientCount
        , _cache       :: !DbCache
        , _options     :: !Options
        , clientHandle :: !Handle
        }
    | ProxyData
        { _options      :: !Options
        , _clientCount  :: !ClientCount
        , _remoteHandle :: !Handle
        , clientHandle  :: !Handle
        }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        defaultPath <- defaultDB
        arguments <- getOptions defaultPath defaultTCPPort <$> getArgs

        case arguments of
            Nothing      -> die usage
            Just options ->
                case _proxy options of
                    Nothing -> serverStartup options
                    Just _  -> proxyStartup options

proxyStartup :: Options -> IO ()
-- ^ proxy server
proxyStartup options = withSocketsDo $ do
        serverContext <- getContext $ Left target
        case serverContext of
            (NetworkConnection h) -> startup h
            _ -> fail $ "Could not connect to server: " <> show target
    where
        target = fromJust $ _proxy options

        startup remote = do
            local <- listenOn $ PortNumber $ _tcpPort options
            putStrLn "Proxy Server started"

            ccMV <- newTVarIO 0           -- running client total
            let env = ProxyData options ccMV remote
            clientForker local env


serverStartup :: Options -> IO ()
-- ^ server
serverStartup opts = do
        db <- (getDB $ _databasePath opts) :: IO Value
        case db of
            Null -> die "Could not parse database on disk"
            _    -> startup db opts
    where
        startup :: Value -> Options -> IO ()
        startup db options = withSocketsDo $ do
            tcpSocket  <- listenOn $ PortNumber $ _tcpPort options
            putStrLn "Server started"

            dbMV <- newTVarIO db          -- in memory database
            chMV <- newTVarIO emptyCache  -- in memory cache
            wrMV <- newTVarIO False       -- do we have writes to flush to disk?
            ccMV <- newTVarIO 0           -- running client total

            let env = ServerData dbMV wrMV ccMV chMV options
            when (_enablePersist options) $
                persistThread $ env stdout

#ifndef mingw32_HOST_OS
            unixSocket <- getUnixSocket
            -- listen on both sockets
            when (_enableUnix options) $
                void . async $
                    clientForker unixSocket env
#endif

            clientForker tcpSocket env

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


clientForker :: Socket -> (Handle -> ThreadData) -> IO b
-- ^ listen for clients, fork off workers
clientForker socket env = forever $ do
        -- do not accept any more clients if we're over our connection limit
        atomically $ do
            count <- readTVar n
            when (count >= maxClients) retry

        -- accept a new client connection, set buffering, increment client total
        (client, _, _) <- accept socket
        hSetBuffering client NoBuffering
        atomically $ modifyTVar n (+ 1)

        -- start client loop to handle queries
        void $ forkFinally
            (clientLoop serve $ env client)
            (cleanup client)
    where
        cleanup client _ = do
            hClose client
            atomically $ modifyTVar n (\x -> x - 1)

        maxClients = 1000
        n = _clientCount $ env stdout


diskWriter :: ThreadData -> IO ()
diskWriter ProxyData{} = fail "unsupported"
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter (ServerData d w _ _ o _) = forever $ do
        threadDelay oneSecond
        needWrite <- readTVarIO w

        when needWrite $ do
            db <- readTVarIO d
            saveDB path db
    where
        oneSecond = 1000000
        path = _databasePath o :: FilePath


clientLoop :: (ThreadData -> ByteString -> IO ()) -> ThreadData -> IO ()
-- ^ read queries from the client, serve them or quit
clientLoop f env = do
        query <- timeout fiveMinutes $ protoRead client
        case join query of
            -- something went wrong reading the query, exit
            Nothing  -> pure ()
            (Just q) -> do
                -- run the query through the database, try to reply to the client
                success <- timeout fiveMinutes $ f env q
                case success of
                    Nothing -> pure ()
                    _       -> clientLoop f env
    where
        client = clientHandle env :: Handle
        fiveMinutes = 60 * 5 * oneSecond
        oneSecond = 1000000


serve :: ThreadData -> ByteString -> IO ()
serve (ProxyData o cc remote client) rawQuery = do
        protoSend remote rawQuery
        protoRead remote >>= \case
            Nothing -> pure ()
            Just reply -> do
                protoSend client reply
                maybeLog cc True True query
    where
        maybeLog :: ClientCount -> Bool -> Bool -> Query -> IO ()
        maybeLog
            | _enableLog o = logToConsole
            | otherwise    = const . const . const . const $ pure ()

        query :: Query
        query = filter (not . T.null) . T.split (== '\n')
              $ decodeUtf8 rawQuery

-- Run a user query through the database, and send them the result.
-- If the database reports that it changed, we set writeNeeded.
serve (ServerData d w cc c o client) rawQuery = do
        cache <- readTVarIO c
        case cacheRead cache query of

            -- cache hit
            Just value -> do
                reply client value
                maybeLog cc cacheHit noChange query

            -- cache miss, or disabled
            Nothing -> do
                value <- atomically $ do

                    -- retrieve database, run action
                    db <- readTVar d
                    let (result, changed, newDB) = runAction db query

                    if changed
                        -- update database, set writeNeeded, clear the cache
                        then do
                            writeTVar d newDB
                            writeTVar w True
                            writeTVar c emptyCache

                        -- no change, just update the cache
                        else writeTVar c $ cachePut cache query result

                    -- pass back result for client
                    pure result

                reply client value
                maybeLog cc cacheMiss noChange query

    where
        maybeLog :: ClientCount -> Bool -> Bool -> Query -> IO ()
        maybeLog
            | _enableLog o = logToConsole
            | otherwise    = const . const . const . const $ pure ()

        cacheRead :: Cache -> Query -> Maybe Text
        cacheRead
            | _enableCache o = cacheGet
            | otherwise      = const . const Nothing

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
