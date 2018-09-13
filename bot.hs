module Main where

import Apocrypha.Client
import Devbot.Core

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)
import Data.List (intercalate)
import System.Process (spawnCommand, getProcessExitCode, ProcessHandle)
import System.Exit (ExitCode(..))

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import qualified Control.Monad

type State = [Task]

data Task = Task
      Event
      (Maybe ProcessHandle)
      StartTime

type StartTime = Integer


startingState :: [Maybe Event] -> State
startingState es =
    map (\e -> Task e Nothing 0) $ convert es

runner :: State -> IO State
runner state = do
    print "running"
    newState <- mapM handle state

    threadDelay (5 * 1000000)
    runner newState


getTime :: IO Integer
getTime = round `fmap` getPOSIXTime


handle :: Task -> IO Task
handle task@(Task (Event c d) Nothing _) = do
    -- not currently running
    time <- getTime

    if ready time d
        then check task
        else return task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ when _) = now > when

handle task@(Task _ (Just h) _) = do
    -- running, see if it's done
    code <- getProcessExitCode h
    case code of
        Nothing          -> return task
        Just ExitSuccess -> success task
        Just _           -> failure task

success :: Task -> IO Task
success (Task event@(Event c d) _ startTime) = do

    setTime c next
    return $ Task (updateTime event next) Nothing 0

    where next = nextRun startTime c

failure :: Task -> IO Task
failure (Task event@(Event c@(Config n _ _ _) d) _ startTime) = do
    -- the command failed, log the error, increment errors and set next
    -- time to retry
    logger $ concat ["running ", n
                    , " failed, backing off "
                    , show backoff, " seconds"]

    -- next <- getTime >>= (return . (+ 1))
    next <- (+ 1) <$> getTime

    setTime c next
    return $ Task (incrementError $ updateTime event next) Nothing 0

    where backoff = getBackoff d

          getBackoff :: Data -> Integer
          getBackoff (Data _ _ Nothing)  = 10
          getBackoff (Data _ _ (Just e)) = (e + 1) * 10

          incrementError :: Event -> Event
          incrementError (Event c (Data d w Nothing)) =
              Event c (Data d w (Just 1))
          incrementError (Event c (Data d w (Just e))) =
              Event c (Data d w (Just $ e + 1))

updateTime :: Event -> Integer -> Event
updateTime (Event c (Data d _ e)) newTime =
  Event c (Data d newTime e)

setTime :: Config -> Integer -> IO ()
setTime (Config n _ _ _) time = do
      _ <- set' ["devbot", "data", n, "when"] $ show time
      return ()

check :: Task -> IO Task
check task@(Task (Event c@(Config n _ _ _) _) _ _) = do
    met <- requirementsMet c

    if met
        then run task
        else do
            logger $ "requirement not met for " ++ n
            return task

run :: Task -> IO Task
run (Task event@(Event (Config n c _ _) _) _ _) = do
    putStrLn $ "Running: " ++ n ++ ": " ++ cmd
    h <- spawnCommand cmd
    time <- getTime
    return (Task event (Just h) time)
    where cmd = intercalate "\n" c


nextRun :: Integer -> Config -> Integer
nextRun time (Config _ _ interval _) =
        time + parse interval
    where
          parse "hourly" = hour
          parse "daily"  = daily
          parse "weekly" = weekly
          parse n        = fromMaybe daily (readMaybe n :: Maybe Integer)

          weekly = daily * 7
          daily  = hour * 24
          hour   = minute * 60
          minute = 60


logger :: String -> IO ()
logger msg = do
    time <- getTime
    putStrLn $ "devbot: " ++ show time ++ " " ++ msg

devbotLog (Config name _ _ _) msg =
    putStrLn $ "devbot " ++ name ++ " " ++ msg


requirementsMet :: Config -> IO Bool
requirementsMet c = return True

convert :: [Maybe a] -> [a]
convert [] = []
convert (Nothing:xs) = convert xs
convert (Just e :xs) = e : convert xs


main :: IO ()
main = do
    putStrLn "devbot starting up"
    es <- events
    _ <- runner $ startingState es
    return ()
