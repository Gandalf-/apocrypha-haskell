module Main where

import System.Environment (getArgs)
import Apocrypha.Client
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad

main = do
        c <- getContext Nothing
        mapM_ (\_ -> keys c []) [1..10000]


readWrite = do
        async produce
        consume

        where produce = withC (\c -> pop c    ["messages", "benchmark"])
              consume = withC (\c -> append c ["messages", "benchmark"] "value")


withC f = do
        c <- getContext Nothing
        forever f c
