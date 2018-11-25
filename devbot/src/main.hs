module Main where

import           System.Environment (getArgs)
import           System.Exit        (die)

import           Devbot.Bot
import           Devbot.List
import           Devbot.Status


main :: IO ()
main = do
        option <- getArgs
        case option of
            []             -> runBot
            ("list" : _)   -> runList
            ("status" : _) -> runStatus
            _              -> die usage


usage :: String
usage = "usage: (<devbot> | list | status)"
