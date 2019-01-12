module Main where

import           Apocrypha.Protocol (client, defaultContext)
import           System.Environment (getArgs)


main :: IO ()
main = do
        c <- defaultContext
        getArgs >>= client c >>= display


display :: Maybe String -> IO ()
display Nothing  = pure ()
display (Just s)
        | null s || s == "\n" = pure ()
        | otherwise           = putStr s
