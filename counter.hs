module Main where

import Apocrypha.Client
import Control.Concurrent (threadDelay)

produce :: Context -> (String -> String) -> IO ()
produce c f = do
    threadDelay $ 1 * second
    value <- pop c index
    next value

    where second = 100000 -- 0
          index  = ["haskell", "counter"]

          next Nothing  = produce c f
          next (Just n) = do
            let v = f n
            print v
            append c index v
            produce c f


increment :: String -> String
increment = reverse

main :: IO ()
main = do
    c <- getContext Nothing
    produce c increment
