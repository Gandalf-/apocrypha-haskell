{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import Network
import System.IO

import Database
import Data.Aeson

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T


type Database = MVar Value
type ServerApp = ReaderT ThreadData IO
data Speaker = Server | Client Text
data ThreadData = ThreadData
                { threadHandle :: Handle
                , userTableMV  :: Database
                }


main :: IO ()
main = do
    server <- listenOn $ PortNumber 5002
    T.putStrLn "Server started"
    db <- getDB Nothing
    case db of
        Null -> putStrLn "Could not parse database on disk"
        _    -> newMVar db >>= clientLoop server


clientLoop :: Socket -> Database -> IO b
clientLoop socket db = do
    (h, _, _) <- accept socket
    hSetBuffering h LineBuffering

    _ <- forkIO $ runReaderT userChat (ThreadData h db)
    clientLoop socket db


userChat :: ServerApp ()
userChat =
    -- name <- addUser
    -- echoLocal name
    forever $ getRemoteLine >>= serve
    -- h <- viewHandle
    -- flip catchError (\_ -> removeUser name) $
    --   do echoLocal $ "Accepted " <> name
    --      forever $ getRemoteLine >>= broadcast (Client name)


{-
removeUser :: Text -> ServerApp ()
removeUser name = do
    echoLocal $ "Exception with " <> name <> ", removing from userTable"
    broadcast Server $ name <> " has left the server"
    modifyUserTable (M.delete name)


addUser :: ServerApp Text
addUser = do
    h <- viewHandle
    dbMV <- viewDatabase
    echoRemote "Enter username"
    name <- T.filter ( /= '\r') <$> getRemoteLine
    userTable <- takeMVarT dbMV

    if name `M.member` userTable
      then do echoRemote "Username already exists!"
              putMVarT dbMV userTable
              addUser

      else do putMVarT dbMV (M.insert name h userTable)
              broadcast Server $ name <> " has joined the server"
              echoRemote "Welcome to the server!\n>> Other users:"
              readMVarT dbMV >>=
                  mapM_ (echoRemote . ("*" <>) . fst)
                . filter (( /= name) . fst) . M.toList
              return name

broadcast :: Speaker -> Text -> ServerApp ()
broadcast user msg =
    viewDatabase >>= readMVarT >>= mapM_ (f . snd) . fn . M.toList
  where f h = liftIO $ T.hPutStrLn h $ nm <> msg
        (fn, nm) = case user of
                    Server -> (id, ">> ")
                    Client t -> (filter ((/=t) . fst), t <> "> ")
-}


echoLocal :: Text -> ServerApp ()
echoLocal = liftIO . T.putStrLn

echoRemote :: Text -> ServerApp ()
echoRemote = echoMessage . (">> " <>)

echoMessage :: Text -> ServerApp ()
echoMessage msg = viewHandle >>= \h -> liftIO . T.hPutStrLn h $ msg

getRemoteLine :: ServerApp Text
getRemoteLine = viewHandle >>= liftIO . T.hGetLine

serve :: Text -> ServerApp ()
serve t = do
    echoLocal $ "query: " <> t
    dbMV <- viewDatabase
    db <- takeMVarT dbMV
    (result, newDB) <- query db t
    echoRemote $ T.pack result
    putMVarT dbMV newDB
    return ()


query :: Value -> Text -> ServerApp (String, Value)
query db t = do
    let (Action newDB output) = action baseAction q
    return (intercalate "\n" output, newDB)
    where
          q :: Operations
          q = map T.unpack . T.split (== ' ') $ t

          baseAction = Action db []

putMVarT  = (liftIO . ) . putMVar
takeMVarT = liftIO . takeMVar
-- readMVarT = liftIO . readMVar

modifyUserTable fn = viewDatabase >>= \mv ->
                     liftIO $ modifyMVar_ mv (return . fn)
viewHandle = threadHandle <$> ask
viewDatabase  = userTableMV  <$> ask
