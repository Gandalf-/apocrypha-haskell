module Main where

import Apocrypha.Client

main = do
        c <- getContext (Just ("127.0.0.1", 8888))
        mapM_ (\x -> keys c []) [1..1000]
        cleanContext c
