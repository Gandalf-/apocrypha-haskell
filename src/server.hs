{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async (async)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson               (Value (..))
import           Data.ByteString.Char8    (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Network
import           System.Directory         (doesFileExist, removeFile)
import           System.Environment       (getArgs)
import           System.Exit              (die)
import           System.IO

import           Apocrypha.Cache          (Cache, emptyCache, get, put)
import           Apocrypha.Database       (Query, defaultDB, getDB, runAction,
                                           saveDB)
import           Apocrypha.Options
import           Apocrypha.Protocol       (protoRead, protoSend, unixSocketPath)


type WriteNeeded = MVar Bool
type Database = MVar Value
type DbCache  = MVar Cache
type ServerApp = ReaderT ThreadData IO

data ThreadData = ThreadData
        { _threadHandle :: !Handle
        , _database     :: !Database
        , _writeNeeded  :: !WriteNeeded
        , _cache        :: !DbCache
        , _options      :: !Options
        }


main :: IO ()
-- ^ set up the listening socket, read the database from disk and start
-- initial worker threads
main = do
        defaultPath <- defaultDB
        arguments <- getOptions defaultPath <$> getArgs

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
            tcpSocket  <- listenOn $ PortNumber 9999
            unixSocket <- getUnixSocket

            putStrLn "Server started"
            dbMV <- newMVar db
            wrMV <- newMVar False
            chMV <- newMVar emptyCache

            when (_enablePersist options) $
                persistThread (ThreadData stdout dbMV wrMV chMV options)

            -- listen on both sockets
            when (_enableUnix options) $
                void . async $
                    clientForker unixSocket dbMV wrMV chMV options

            clientForker tcpSocket dbMV wrMV chMV options

        persistThread :: ThreadData -> IO ()
        persistThread = void . async . runReaderT diskWriter

        getUnixSocket :: IO Socket
        getUnixSocket = do
            exists <- doesFileExist unixSocketPath
            when exists $
                removeFile unixSocketPath
            listenOn $ UnixSocket unixSocketPath


clientForker :: Socket -> Database -> WriteNeeded -> DbCache -> Options -> IO b
-- ^ listen for clients, fork off workers
clientForker socket d w c o = forever $ do
        (h, _, _) <- accept socket
        hSetBuffering h NoBuffering
        forkerThread (ThreadData h d w c o)
    where
        forkerThread = void . async . runReaderT clientLoop


diskWriter :: ServerApp ()
-- ^ checks if a write to disk is necessary once per second
-- done in a separate thread so client threads can run faster
diskWriter = forever $ do
        write <- readMVarT =<< viewWrite
        db <- readMVarT =<< viewDatabase
        path <- _databasePath <$> viewOptions

        liftIO $ threadDelay oneSecond
        when write $ liftIO (saveDB path db)
    where
        oneSecond = 1000000


clientLoop :: ServerApp ()
-- ^ read queries from the client, serve them or quit
clientLoop =
        flip catchError (\_ -> pure ()) $ do
              query <- getQuery
              case query of
                    Nothing  -> pure ()
                    (Just q) -> serve q >> clientLoop


getQuery :: ServerApp (Maybe ByteString)
-- ^ Read a client query from our network handle
getQuery = viewHandle >>= liftIO . protoRead


serve :: ByteString -> ServerApp ()
-- ^ Run a user query through the database, and send them the result.
-- If the database reports that it changed, we set writeNeeded.
serve rawQuery = do

        cache <- takeMVarT =<< viewCache
        cacheEnabled <- _enableCache <$> viewOptions

        case get cacheEnabled cache query of

            -- cache hit
            Just value -> do
                putMVarT cache =<< viewCache

                replyToClient value =<< viewHandle
                logToConsole cacheHit noChange query

            -- cache miss, or disabled
            Nothing -> do
                db <- takeMVarT =<< viewDatabase

                let (result, changed, newDB) = runAction db query
                    newCache = put cache query result

                putMVarT newDB    =<< viewDatabase
                putMVarT newCache =<< viewCache

                replyToClient result =<< viewHandle
                logToConsole cacheMiss changed query

                when changed $ do
                    setWriteNeeded
                    clearCache
    where
        query :: Query
        query = filter (not . T.null)
              . T.split (== '\n')
              $ decodeUtf8 rawQuery

        cacheHit = True
        cacheMiss = False
        noChange = False


setWriteNeeded :: ServerApp ()
setWriteNeeded = do
    _ <- takeMVarT =<< viewWrite
    putMVarT True =<< viewWrite


clearCache :: ServerApp ()
clearCache = do
    _ <- takeMVarT =<< viewCache
    putMVarT emptyCache =<< viewCache


replyToClient :: Text -> Handle -> ServerApp ()
replyToClient value h = liftIO . protoSend h . encodeUtf8 $ value


logToConsole :: Bool -> Bool -> Query -> ServerApp ()
-- ^ write a summary of the query to stdout
logToConsole hit write query = do
        enableLog <- _enableLog <$> viewOptions
        when enableLog $
            echoLocal . T.take 80 $ status <> T.unwords query
    where
        status
            | hit && write = "? "       -- this shouldn't happen
            | hit          = "  "
            | write        = "~ "
            | otherwise    = "* "       -- no hit, no write


-- | MVar Utilities

putMVarT :: a -> MVar a -> ReaderT ThreadData IO ()
putMVarT thing place = liftIO $ putMVar place thing

readMVarT :: MVar a -> ReaderT ThreadData IO a
readMVarT = liftIO . readMVar

takeMVarT :: MVar a -> ReaderT ThreadData IO a
takeMVarT = liftIO . takeMVar


-- | ReaderT Utilities

viewHandle :: ReaderT ThreadData IO Handle
viewHandle = asks _threadHandle

viewDatabase :: ReaderT ThreadData IO Database
viewDatabase = asks _database

viewWrite :: ReaderT ThreadData IO WriteNeeded
viewWrite = asks _writeNeeded

viewCache :: ReaderT ThreadData IO DbCache
viewCache = asks _cache

viewOptions :: ReaderT ThreadData IO Options
viewOptions = asks _options

echoLocal :: Text -> ReaderT ThreadData IO ()
echoLocal = liftIO . putStrLn . T.unpack
