{-# LANGUAGE OverloadedStrings #-}

module ProtocolSpec (spec) where

import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           GHC.IO.Handle.Types   (Handle)
import           System.Directory
import           System.IO
import           Test.Hspec
import           Test.QuickCheck

import           Apocrypha.Protocol


instance Arbitrary B8.ByteString where
        arbitrary = B8.pack <$> arbitrary

tmpFile :: FilePath
tmpFile = "/tmp/apocrypha.test"

cleanUp :: IO ()
cleanUp = do
      exists <- doesFileExist tmpFile
      when exists $ removeFile tmpFile


huge :: B8.ByteString
huge = B8.pack $ take (1024 * 1024) $ cycle ['A'..'z']


spec :: Spec
spec = do
        describe "protocol" $
          it "encoding a message makes it longer" $
            property $ \x ->
              B8.length (protocol x) > B8.length (x :: B8.ByteString)

        describe "protocol" $
          it "identity. read what we wrote and making sure it's the same" $
            property $ \x -> do
              cleanUp
              result <- withBinaryFile tmpFile ReadWriteMode (readWrite x)
              result `shouldBe` Just (x :: B8.ByteString)

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


readWrite :: B8.ByteString -> Handle -> IO (Maybe B8.ByteString)
readWrite bytes h = do
        protoSend h bytes
        resetHandle h
        protoRead h


tinyReadWrite :: Handle -> IO (Maybe B8.ByteString)
tinyReadWrite h = do
        hPutStr h "aaa"
        resetHandle h
        protoRead h
