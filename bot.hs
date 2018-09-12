module Main where

import Devbot.Core

import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (threadDelay, forkIO)
import Data.List (intercalate)
import System.Process (spawnCommand, waitForProcess, ProcessHandle)

import qualified Control.Monad

getTime :: IO Integer
getTime = round `fmap` getPOSIXTime


-- handle :: Integer -> Maybe Event -> IO ()
handle now (Event c d) =
    if ready now d
      then do
        h <- run (Event c d)
        return $ Just h
      else return Nothing


-- run :: Event -> IO ()
run (Event (Config name actions interval requirements) _) = do
    print name
    let cmd = "echo " ++ name -- intercalate "\n" actions
    h <- spawnCommand cmd
    _ <- forkIO (cleanup h)
    return ()


cleanup :: ProcessHandle -> IO ()
cleanup ph = do
    _ <- waitForProcess ph
    return ()


ready :: Integer -> Data -> Bool
ready now (Data _ when _) =
    when > now


loop :: [Event] -> IO ()
loop es = do
    time <- getTime
    mapM_ (handle time) es

    threadDelay (5*1000000)

    loop es


convert :: [Maybe a] -> [a]
convert [] = []
convert (Nothing:xs) = convert xs
convert (Just e :xs) = e : convert xs


main :: IO ()
main = do
    es <- events
    loop $ convert es
