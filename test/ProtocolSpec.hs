{-# LANGUAGE OverloadedStrings #-}

module ProtocolSpec (spec) where

import           Apocrypha.Protocol

import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           GHC.IO.Handle.Types   (Handle)
import           System.Directory
import           System.IO
import           Test.Hspec
import           Test.QuickCheck


instance Arbitrary BS.ByteString where
        arbitrary = BS.pack <$> arbitrary

tmpFile :: FilePath
tmpFile = "/tmp/apocrypha.test"

cleanUp :: IO ()
cleanUp = do
      exists <- doesFileExist tmpFile
      when exists $ removeFile tmpFile


huge :: BS.ByteString
huge = BS.pack $ take (1024 * 1024) $ cycle ['A'..'z']


spec :: Spec
spec = do
        describe "protocol" $
          it "encoding a message makes it longer" $
            property $ \x ->
              BS.length (protocol x) > BS.length (x :: BS.ByteString)

        describe "protocol" $
          it "identity. read what we wrote and making sure it's the same" $
            property $ \x -> do
              cleanUp
              result <- withBinaryFile tmpFile ReadWriteMode (readWrite x)
              result `shouldBe` Just (x :: BS.ByteString)

        describe "protocol" $
          it "huge write, read. tests chunked reading and writing" $ do
            cleanUp
            result <- withBinaryFile tmpFile ReadWriteMode (readWrite huge)
            result `shouldBe` Just huge

        describe "protocol" $
          it "less than 4 bytes available" $ do
            cleanUp
            result <- withBinaryFile tmpFile ReadWriteMode tinyReadWrite
            result `shouldBe` Nothing


resetHandle :: Handle -> IO ()
resetHandle h = hFlush h >> hSeek h AbsoluteSeek 0


readWrite :: BS.ByteString -> Handle -> IO (Maybe BS.ByteString)
readWrite bytes h = do
        protoSend h bytes
        resetHandle h
        protoRead h


tinyReadWrite :: Handle -> IO (Maybe BS.ByteString)
tinyReadWrite h = do
        hPutStr h "aaa"
        resetHandle h
        protoRead h
