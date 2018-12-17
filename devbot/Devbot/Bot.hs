module Devbot.Bot
    ( runBot
    ) where

import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever)
import           Data.List             (intercalate)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.Exit           (ExitCode (..))
import           System.IO             (BufferMode (..), hSetBuffering, stdout)
import           System.Process        (ProcessHandle, getProcessExitCode,
                                        spawnCommand, waitForProcess)

import           Apocrypha.Client
import           Devbot.Core


type State = [Task]

data Task = Task
          { _event   :: Event
          , _process :: Maybe ProcessHandle
          , _start   :: Integer
          }


runBot :: IO ()
runBot = do
        putStrLn "devbot starting up"
        hSetBuffering stdout LineBuffering

        forever $
          events >>= runner 1 . startingState


startingState :: [Event] -> State
startingState = map (\ event -> Task event Nothing 0)


runner :: Integer -> State -> IO State
-- ^ check each Task for something to do
-- run for n iterations before dropping out so main can refetch the
-- events and start us again
runner runs state =
        if runs > minRunsToRestart && noRunners state
          then pure []
          else do
              threadDelay $ 1 * second
              mapM handle state >>= runner (runs + 1)

    where
        second  = 1000000
        minRunsToRestart = 60 * 5

        noRunners :: State -> Bool
        noRunners []                       = True
        noRunners (Task _ Nothing  _ : xs) = noRunners xs
        noRunners (Task _ (Just _) _ : _ ) = False


handle :: Task -> IO Task
handle task@(Task (Event _ _ d) Nothing _) = do
        -- not currently running
        time <- getTime

        if ready time d
            then check task
            else pure task
    where
        ready :: Integer -> Data -> Bool
        ready now (Data _ _when _) = now > _when

handle task@(Task _ (Just h) _) = do
        code <- getProcessExitCode h
        case code of
            -- still running
            Nothing          -> pure task

            -- finished
            Just ExitSuccess -> success task
            Just _           -> failure task


check :: Task -> IO Task
check task@(Task (Event n c d) p s) = do
        -- if we can't run, wait 30 seconds before trying again
        now <- getTime
        met <- requirementsMet n c
        if met
            then run task
            else pure $ Task (Event n c (backoff now d)) p s
    where
        backoff :: Integer -> Data -> Data
        backoff now (Data _d _ _e) = Data _d (now + 30) _e


run :: Task -> IO Task
run (Task event@(Event _ (Config c _ _) _) _ _) = do
        -- start running the actions, add handle to Task
        -- putStrLn $ "Running: " ++ n ++ ":\n" ++ cmd
        h <- spawnCommand cmd
        Task event (Just h) <$> getTime
    where
        cmd = intercalate "\n" c


success :: Task -> IO Task
success (Task event@(Event _ c _) _ startTime) = do
        -- the command succeeded, set errors to Nothing, and determine next
        -- time to run
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = clearErrors $ updateTime event next elapsed
            newTask  = Task newEvent Nothing 0

        flush newEvent
        pure newTask
    where
        next = nextRun startTime c

        clearErrors :: Event -> Event
        clearErrors (Event n co (Data du wh _)) =
            Event n co (Data du wh Nothing)


failure :: Task -> IO Task
failure (Task event@(Event n _ d) _ startTime) = do
        -- the command failed, log the error, increment errors and set next
        -- time to retry
        logger $ concat ["running ", n
                        , " failed, backing off "
                        , show backoff, " seconds"]

        -- next <- getTime >>= (pure . (+ 1))
        next <- (+ backoff) <$> getTime
        elapsed <- negate . (startTime -) <$> getTime

        let newEvent = incrementError $ updateTime event next elapsed
            newTask  = Task newEvent Nothing 0

        flush newEvent
        pure newTask
    where
        backoff = getBackoff d

        getBackoff :: Data -> Integer
        getBackoff (Data _ _ Nothing)  = 10
        getBackoff (Data _ _ (Just e)) = (e + 1) * 10

        incrementError :: Event -> Event
        incrementError (Event _n _c (Data _d _w Nothing)) =
            Event _n _c (Data _d _w (Just 1))

        incrementError (Event _n _c (Data _d _w (Just e))) =
            Event _n _c (Data _d _w (Just $ e + 1))


updateTime :: Event -> Integer -> Integer -> Event
updateTime (Event n c (Data _ _ e)) newTime elapsed =
        Event n c (Data elapsed newTime e)


nextRun :: Integer -> Config -> Integer
nextRun time (Config _ _interval _) = time + _interval


flush :: Event -> IO ()
flush (Event n _ d) = set' ["devbot", "data", n] d


logger :: String -> IO ()
logger msg = do
        time <- getTime
        putStrLn $ "devbot: " ++ show time ++ " " ++ msg


requirementsMet :: String -> Config -> IO Bool
requirementsMet _ (Config _ _ Nothing) = pure True
requirementsMet n (Config _ _ (Just r)) = do
        req <- get' ["devbot", "requirements", r]

        case req of
            Nothing  -> logger doesntExist >> pure False
            (Just a) -> runCheck a
    where
        runCheck :: String -> IO Bool
        runCheck cmd = do
            code <- spawnCommand cmd >>= waitForProcess
            case code of
                ExitSuccess -> pure True
                _           -> logger cmdFailed >> pure False

        doesntExist =
          n ++ " references requirement that doesn't exist"

        cmdFailed =
          "requirement " ++ r ++ " for " ++ n ++ " not met"


getTime :: IO Integer
getTime = round <$> getPOSIXTime
