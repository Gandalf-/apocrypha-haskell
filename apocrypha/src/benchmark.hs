module Main where

import           Apocrypha.Client
import           Control.Concurrent.Async
import qualified Data.HashMap.Lazy        as HM
import           Data.List                (intercalate, sortOn)
import           Data.Maybe               (fromMaybe)
import           Data.Ord                 (Down (..))
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
bench f = do
        start <- getPOSIXTime
        _ <- f
        end <- getPOSIXTime
        print (end - start)


testCaseMap :: HM.HashMap String (IO ())
-- ^ this is intentionally a lazy hashmap, so we don't evaluate early
testCaseMap = HM.fromList
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
-- look up and run a test case, otherwise show the available tests
run test =
        fromMaybe showUsage testCase
    where
        testCase :: Maybe (IO ())
        testCase = HM.lookup test testCaseMap

        showUsage :: IO ()
        showUsage = die
            $ ("options:\n  " ++)
            $ intercalate "\n  "
            $ sortOn Down
            $ HM.keys testCaseMap

type HashMapEntry = (String, IO ())

runAll :: IO ()
-- ^ run all test cases, except this one
runAll =
        mapM_ runner $ filter notSelf $ HM.toList testCaseMap
    where
        notSelf :: HashMapEntry -> Bool
        notSelf (name, _) = name /= "all-tests"

        runner :: HashMapEntry -> IO ()
        runner (name, f) =
            putStrLn name >> f >> putStrLn ""


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
