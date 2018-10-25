module Main where

import           Apocrypha.Protocol (client, getContext)
import           System.Environment (getArgs)


display :: Maybe String -> IO ()
display Nothing  = return ()
display (Just s) =
        if null s || s == "\n"
            then return ()
            else putStr s


main :: IO ()
main = do
        c <- getContext Nothing
        getArgs >>= client c >>= display
