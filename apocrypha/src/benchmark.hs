module Main where

import           Apocrypha.Client
import           Control.Concurrent.Async
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Environment       (getArgs)
import           System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> die "usage: [test name]"
            xs -> run $ head xs


bench :: IO a -> IO ()
bench f = do
        start <- getPOSIXTime
        _ <- f
        end <- getPOSIXTime
        print (end - start)


run :: String -> IO ()
run "single-reader"       = bench singleReader
run "single-writer"       = bench singleWriter
run "single-reader-cache" = bench singleReaderCache

run "multi-reader"        = bench $ multiReader 10
run "multi-reader-cache"  = bench $ multiReaderCache 10

run "many-reader"         = bench $ multiReader 20
run "many-reader-cache"   = bench $ multiReaderCache 20
run _                     = die "unknown test"


singleCount :: Int
singleCount = 100000


singleReader :: IO ()
-- read a different value each time
singleReader = do
        c <- defaultContext
        mapM_ (\v -> keys c [show v]) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount

singleWriter :: IO ()
-- ^ write a different value each time
singleWriter = do
        c <- defaultContext
        mapM_ (set c ["benchmark"] . show) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount


singleReaderCache :: IO ()
-- read the same value each time
singleReaderCache = do
        c <- defaultContext
        mapM_ (\_ -> keys c []) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount


multiReader :: Int -> IO ()
multiReader count = do
        mapM_ (\ _ -> async singleReader) iters
        singleReader
    where
        iters = [1..count - 1] :: [Int]

multiReaderCache :: Int -> IO ()
multiReaderCache count = do
        mapM_ (\ _ -> async singleReaderCache) iters
        singleReaderCache
    where
        iters = [1..count - 1] :: [Int]
