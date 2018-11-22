module Main where

import           Apocrypha.Client
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Environment       (getArgs)
import           System.Exit

main = do
        args <- getArgs
        case args of
            [] -> die "usage: [test name]"
            xs -> run $ head xs


bench f = do
        start <- getPOSIXTime
        f
        end <- getPOSIXTime
        print (end - start)


run :: String -> IO ()
run "single-reader" = bench singleReader
run "multi-reader"  = bench multiReader
run "single-writer" = bench singleWriter


singleCount = 100000


singleReader :: IO ()
singleReader = do
        c <- defaultContext
        mapM_ (\v -> keys c [show v]) [1..count]
    where
        count = singleCount

multiReader :: IO ()
multiReader = do
        mapM_ (\ _ -> async singleReader) [1..count - 1]
        singleReader
    where
        count = 10

singleWriter :: IO ()
singleWriter = do
        c <- defaultContext
        mapM_ (set c ["benchmark"] . show) [1..count]
    where
        count = singleCount
