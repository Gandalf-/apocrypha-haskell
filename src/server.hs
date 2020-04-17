{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async    (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (finally)
import           Control.Monad               (join)
import           Control.Monad.Except
import           Control.Monad.STM
import           Data.Aeson                  (Value (..))
import           Data.ByteString.Char8       (ByteString)
import           Data.Maybe                  (fromMaybe)
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

type ClientCount = TVar Integer

class Server a where
    logEnabled   :: a -> Bool
    clientCount  :: a -> ClientCount
    clientHandle :: a -> Handle
    serve        :: a -> ByteString -> IO ()

data Database = Database
    { _database           :: !(TVar Value)
    , _writeNeeded        :: !(TVar Bool)
    , _serverClientCount  :: !ClientCount
    , _cache              :: !(TVar Cache)
    , _serverOptions      :: !DatabaseOptions
    , _serverClientHandle :: !Handle
    }

data Proxy = Proxy
    { _proxyOptions      :: !ProxyOptions
    , _proxyClientCount  :: !ClientCount
    , _remoteHandle      :: !(TVar Handle)
    , _remoteLock        :: !(TVar Bool)
    , _proxyClientHandle :: !Handle
    }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        defaultPath <- defaultDB

        parseOptions defaultPath defaultTCPPort <$> getArgs >>=
            maybe
                (die usage)
                (either databaseStartup proxyStartup)

proxyStartup :: ProxyOptions -> IO ()
-- ^ proxy server
proxyStartup options = withSocketsDo $
        remoteConnect target >>= maybe
            (fail $ "Could not connect to server: " <> show target)
            startup
    where
        target = _proxyTarget options

        startup remote = do
            local <- listenOn $ PortNumber $ tcpPort options
            putStrLn "Proxy Server started"

            ccMV <- newTVarIO 0           -- running client total
            lock <- newTVarIO False       -- lock to protect our remote connection
            handle <- newTVarIO remote    -- tvar so proxyRequest can replace it
            let env = Proxy options ccMV handle lock

            clientForker local env

databaseStartup :: DatabaseOptions -> IO ()
-- ^ database server
databaseStartup opts = do
        db <- (getDB $ _dbPath opts) :: IO Value
        case db of
            Null -> die "Could not parse database on disk"
            _    -> startup db opts
    where
        startup :: Value -> DatabaseOptions -> IO ()
        startup db options = withSocketsDo $ do
            tcpSocket  <- listenOn $ PortNumber $ tcpPort options
            putStrLn "Server started"

            dbMV <- newTVarIO db          -- in memory database
            chMV <- newTVarIO emptyCache  -- in memory cache
            wrMV <- newTVarIO False       -- do we have writes to flush to disk?
            ccMV <- newTVarIO 0           -- running client total

            let env = Database dbMV wrMV ccMV chMV options
            when (_dbEnablePersist options) $
                persistThread $ env stdout

#ifndef mingw32_HOST_OS
            unixSocket <- getUnixSocket
            -- listen on both sockets
            when (_dbEnableUnix options) $
                void . async $
                    clientForker unixSocket env
#endif

            clientForker tcpSocket env

        persistThread :: Database -> IO ()
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

instance Server Database where
    logEnabled   = headless . _serverOptions
    clientCount  = _serverClientCount
    clientHandle = _serverClientHandle

    -- Run a user query through the database, and send them the result.
    -- If the database reports that it changed, we set writeNeeded.
    serve s@(Database d w _ c o client) rawQuery = do
            cache <- readTVarIO c
            case cacheRead cache query of
                -- cache hit
                Just value -> do
                    sendReply client value
                    maybeLog s cacheHit noChange query

                -- cache miss, or disabled
                Nothing -> do
                    runQuery cache >>= sendReply client
                    maybeLog s cacheMiss noChange query
        where
            runQuery :: Cache -> IO Text
            runQuery cache = atomically $ do
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

            cacheRead :: Cache -> Query -> Maybe Text
            cacheRead
                | _dbEnableCache o = cacheGet
                | otherwise        = const . const Nothing

            query :: Query
            query = filter (not . T.null) . T.split (== '\n')
                $ decodeUtf8 rawQuery

            (cacheHit, cacheMiss, noChange) = (True, False, False)

instance Server Proxy where
    logEnabled   = headless . _proxyOptions
    clientCount  = _proxyClientCount
    clientHandle = _proxyClientHandle

    -- proxy serve loop, send the query to the remote, read the reply, send that to the
    -- client as the response. the remote connection is locked while we're making a
    -- request, but unlocked during the reply to the client
    serve s rawQuery = do
            proxyRequest s (`protoSend` rawQuery)
            proxyRequest s protoRead >>= maybe
                (pure ())
                (\reply -> do
                    protoSend client reply
                    maybeLog s True True query)
        where
            client = clientHandle s
            query :: Query
            query = filter (not . T.null) . T.split (== '\n')
                $ decodeUtf8 rawQuery


-- | Server utilities

clientForker :: Server a => Socket -> (Handle -> a) -> IO b
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
        n = clientCount $ env stdout

clientLoop :: Server a => (a -> ByteString -> IO ()) -> a -> IO ()
-- ^ read queries from the client, serve them or quit
clientLoop f env = do
        query <- timeout fiveMinutes $ protoRead client
        case join query of
            -- something went wrong reading the query, exit
            Nothing  -> pure ()
            (Just q) -> do
                -- run the query through the function, which should reply
                success <- timeout fiveMinutes $ f env q
                case success of
                    Nothing -> pure ()
                    _       -> clientLoop f env
    where
        client = clientHandle env :: Handle
        fiveMinutes = 60 * 5 * oneSecond

maybeLog :: Server a => a -> Bool -> Bool -> Query -> IO ()
-- ^ write a summary of the query to stdout
maybeLog s hit write query =
        when (logEnabled s) $ do
            count <- readTVarIO $ clientCount s
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


-- | Proxy utilities

proxyRequest :: Proxy -> (Handle -> IO a) -> IO a
-- ^ wrapper around durableRequest that saves all the state back into the ThreadData for
-- the proxy. this also enforces locking around the proxy's remote handle
proxyRequest d f = lockedIO lock $ do
        h <- readTVarIO th
        (newH, out) <- durableRequest h reconnect f
        atomically $ writeTVar th newH
        pure out
    where
        th = _remoteHandle d
        reconnect = remoteConnect $ _proxyTarget $ _proxyOptions d
        lock = _remoteLock d


-- | Database utiltiies

diskWriter :: Database -> IO ()
-- ^ checks if a write to disk is necessary once per second done in a separate thread so
-- client threads can run faster by not having to block on disk IO
diskWriter (Database d w _ _ o _) = forever $ do
        threadDelay oneSecond
        needWrite <- readTVarIO w

        when needWrite $ do
            db <- readTVarIO d
            saveDB path db
    where
        path = _dbPath o :: FilePath


-- | General utilities

oneSecond :: Int
-- ^ nanoseconds to second
oneSecond = 1000000

isHandleDead :: Handle -> IO Bool
-- ^ potentially block for 100 nanoseconds to tell if the handle is alive. it's not
-- clear if smaller values are safe for remote connections.
isHandleDead h = fromMaybe False <$> timeout 100 (hIsEOF h)

durableRequest :: Handle -> IO (Maybe Handle) -> (Handle -> IO a) -> IO (Handle, a)
-- ^ attempt to make a request given the provided handle. if the handle is dead, the
-- reconnect function provided is used and the request is re-attempted after a delay.
-- the handle returned may not be the handle provided
durableRequest = work 0
    where
        work attempts h reconnect f = do
            dead <- isHandleDead h
            if dead
                then do
                    let next  = attempts + 1
                        delay = attempts * 10

                    when (attempts > 0) $
                        putStrLn $ "reconnecting after " <> show delay <> " seconds"

                    threadDelay $ oneSecond * delay
                    reconnect >>= maybe
                        (work next h reconnect f)
                        (\n -> work next n reconnect f)
                else do
                    when (attempts > 1) $ putStrLn "reconnected"
                    out <- f h
                    pure (h, out)

lockedIO :: TVar Bool -> IO a -> IO a
-- ^ run some IO with a lock (TVar bool)
lockedIO lock f = finally (acquire >> f) release
    where
        acquire = atomically $ do
            locked <- readTVar lock
            when locked retry
            writeTVar lock True
        release = atomically $ writeTVar lock False

sendReply :: Handle -> Text -> IO ()
-- ^ TODO no timeout protection here
sendReply h value = protoSend h $ encodeUtf8 value

remoteConnect :: (String, PortNumber) -> IO (Maybe Handle)
-- ^ attempt to connect to the target in the options, producing a raw handle
remoteConnect target = do
        serverContext <- getContext $ Left target
        pure $ case serverContext of
            (NetworkConnection h) -> Just h
            _                     -> Nothing
