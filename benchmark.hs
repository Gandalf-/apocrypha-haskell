module Main where

import System.Environment (getArgs)
import Apocrypha.Client
import Control.Concurrent.Async
import Control.Concurrent

main = do
        async consume
        async produce
        async produce
        async consume
        consume


worker args a b = do
        keys a $ args ++ ["devbot"]
        keys b $ args ++ ["links"]

consume = do
        c <- getContext Nothing
        result <- mapM (\_ -> pop c ["messages", "benchmark"]) [1..1000]
        print . length $ result
        cleanContext c

produce = do
        c <- getContext Nothing
        result <- mapM (\_ -> append c ["messages", "benchmark"] "value") [1..1000]
        print . length $ result
        cleanContext c
