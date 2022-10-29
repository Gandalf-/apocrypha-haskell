module Main where

import           Apocrypha.Client
import           Control.Concurrent.Async
import           Data.List                (intercalate)
import qualified Data.Map.Lazy            as Map
import           Data.Maybe               (fromMaybe)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           System.Environment       (getArgs)
import           System.Exit

main :: IO ()
main = do
        args <- getArgs
        case args of
            [] -> run "--help"
            xs -> run $ head xs


bench :: IO a -> IO ()
-- ^ time a computation, print the result to stdout
bench f = do
        start <- getPOSIXTime
        _ <- f
        end <- getPOSIXTime
        print (end - start)


testCaseMap :: Map.Map String (IO ())
-- ^ this is intentionally a lazy hashmap, so we don't evaluate early
testCaseMap = Map.fromList
    [ ("single-reader", bench singleReader)
    , ("single-writer", bench singleWriter)
    , ("single-reader-cache", bench singleReaderCache)

    , ("multi-reader", bench $ multiReader 10)
    , ("multi-reader-cache", bench $ multiReaderCache 10)

    , ("many-reader", bench $ multiReader 20)
    , ("many-reader-cache", bench $ multiReaderCache 20)

    , ("all-tests", runAll)
    ]


run :: String -> IO ()
-- ^ look up and run a test case, otherwise show the available tests
run test =
        fromMaybe showUsage testCase
    where
        testCase :: Maybe (IO ())
        testCase = Map.lookup test testCaseMap

        showUsage :: IO ()
        showUsage = die
            $ ("options:\n  " ++)
            $ intercalate "\n  "
            $ reverse
            $ Map.keys testCaseMap

type HashMapEntry = (String, IO ())

runAll :: IO ()
-- ^ run all test cases, except this one
runAll =
        mapM_ runner $ filter notSelf $ Map.toList testCaseMap
    where
        notSelf :: HashMapEntry -> Bool
        notSelf (name, _) = name /= "all-tests"

        runner :: HashMapEntry -> IO ()
        runner (name, f) =
            putStrLn name >> f >> putStrLn ""


singleCount :: Int
singleCount = 100000


benchContext :: IO Context
benchContext = getContext (ServerlessCaching "db.json")


singleReader :: IO ()
-- ^ read a different value each time
singleReader = do
        c <- benchContext
        mapM_ (\v -> keys c [show v]) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount

singleWriter :: IO ()
-- ^ write a different value each time
singleWriter = do
        c <- benchContext
        mapM_ (set c ["benchmark"] . show) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount

singleReaderCache :: IO ()
-- ^ read the same value each time
singleReaderCache = do
        c <- benchContext
        mapM_ (\_ -> keys c []) iters
    where
        iters = [1..count] :: [Int]
        count = singleCount


multiReader :: Int -> IO ()
-- ^ spawn a number of single readers that hit different values
multiReader count = do
        mapM_ (\ _ -> async singleReader) iters
        singleReader
    where
        iters = [1..count - 1] :: [Int]

multiReaderCache :: Int -> IO ()
-- ^ spawn a number of single readers that hit the same value every time
multiReaderCache count = do
        mapM_ (\ _ -> async singleReaderCache) iters
        singleReaderCache
    where
        iters = [1..count - 1] :: [Int]
