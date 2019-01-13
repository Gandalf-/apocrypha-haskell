module Main where

import           Apocrypha.Protocol (client, defaultContext)
import           System.Environment (getArgs)
import           System.IO          (hIsTerminalDevice, stdin)


main :: IO ()
main = do
        c <- defaultContext
        arguments >>= client c >>= display


arguments :: IO [String]
-- ^ if data is piped in, use it as the final argument
arguments = do
        terminal <- hIsTerminalDevice stdin
        if terminal
          then getArgs
          else do
            content <- getContents
            (<> [content]) <$> getArgs


display :: Maybe String -> IO ()
display Nothing  = pure ()
display (Just s)
        | null s || s == "\n" = pure ()
        | otherwise           = putStr s
