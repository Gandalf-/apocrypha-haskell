module Main where

import ColorText
import Devbot.Core

import Text.Read (readMaybe)
import Data.List (intersperse)
import Data.Time.Clock.POSIX (getPOSIXTime)

now :: IO Integer
now = round `fmap` getPOSIXTime

printAction :: Config -> String
printAction c = decorate a blue
    where a = "    " ++ (concat $ intersperse pad $ action c)
          blue = (Blue, Black, Null) :: Decoration
          pad = "\n    "

printName :: Config -> String
printName c = decorate n green
    where n = name c
          green = (Green, Black, Bold) :: Decoration

prettyTime :: Integer -> String
prettyTime i 
    | i <= minute = show i ++ " seconds"
    | i <= hour   = show (div i minute) ++ " minutes"
    | i <= day    = show (div i hour) ++ " hours"
    | otherwise   = show (div i day) ++ " days"
    where day = 86400
          hour = 3600
          minute = 60

secondsOrTime :: String -> Maybe Integer -> String
secondsOrTime d Nothing   = d
secondsOrTime _ (Just i) = "every " ++ s
    where s = prettyTime i

printInterval :: Config -> String
printInterval c = decorate ("    " ++ i) cyan
    where i = secondsOrTime plain (readMaybe plain :: Maybe Integer)
          plain = interval c
          cyan = (Cyan, Black, Null) :: Decoration

printNext :: Data -> Integer -> String
printNext d time = decorate ("next in " ++ n) yellow
    where n = prettyTime $ when d - time
          yellow = (Yellow, Black, Null)


printOptional :: Config -> Data -> IO ()
printOptional c d = do
    printErrors e
    printRequire r
    putStrLn ""
    where 
          printErrors :: Maybe Integer -> IO ()
          printErrors Nothing = return ()
          printErrors (Just s) = 
                putStr $ ", " ++ decorate (show s ++ " errors") red

          printRequire :: Maybe String -> IO ()
          printRequire Nothing = return ()
          printRequire (Just r') =
              putStr $ ", requires " ++ r'

          red = (Red, Black, Null) :: Decoration

          e = errors d
          r = require c

printEvent :: Maybe Event -> IO ()
printEvent Nothing = return ()
printEvent (Just e) = do
    putStrLn $ printName c
    putStrLn $ printAction c
    putStr $ printInterval c
    time <- now
    putStr $ ", " ++ printNext d time
    printOptional c d
    putStrLn ""
    where c = eventConfig e
          d = eventData e

main :: IO ()
main = do
    es <- events
    mapM_ printEvent es
