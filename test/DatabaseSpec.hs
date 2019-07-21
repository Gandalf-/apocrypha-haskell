{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import           Apocrypha.Database
import           Apocrypha.Client
import           Apocrypha.Internal.Database

import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import           Control.Monad
import           Data.Text                   (Text)
import qualified Data.Vector                 as V
import           System.Directory
import           Test.Hspec


spec :: Spec
spec = do

        -- context
        describe "addContext" $
            it "no context" $
              addContext [] "apple" `shouldBe` ["apple"]

        describe "addContext" $
            it "one context" $
              addContext ["a"] "apple" `shouldBe` ["apple"]

        describe "addContext" $
            it "two contexts" $
              addContext ["a", "b"] "apple" `shouldBe` ["a = apple"]

        describe "addContext" $
            it "three contexts" $
              addContext ["a", "b", "c"] "apple" `shouldBe` ["a = b = apple"]

        describe "addContext" $
            it "spaced contexts" $
              addContext ["a b", "c"] "apple" `shouldBe` ["a b = apple"]


        -- get
        describe "get" $
            it "get nothing result" $
              output keyDB ["non-existant"] `shouldBe` ("", False)

        describe "get" $
            it "get something" $
              output keyDB ["apple"] `shouldBe` ("sauce", False)


        -- keys
        describe "keys" $
            it "keys nothing" $
              output keyDB ["non-existant"] `shouldBe` ("", False)

        describe "keys" $
            it "keys something" $
              output keyDB ["--keys"] `shouldBe` ("apple\nblue", False)

        describe "keys" $
            it "keys something alias" $
              output keyDB ["-k"] `shouldBe` ("apple\nblue", False)

        describe "keys" $
            it "keys invalid, on value" $
              output keyDB ["apple", "--keys"] `shouldBe`
                (keyTypeError, False)

        describe "keys" $
            it "keys invalid, on array" $
              output arrayDB ["array", "--keys"] `shouldBe`
                (keyTypeError, False)


        -- pop
        describe "pop" $
            it "pop nothing" $
              output arrayDB ["non-existant", "--pop"] `shouldBe` ("", False)

        describe "pop" $
            it "pop something" $
              output arrayDB ["array", "--pop"] `shouldBe` ("1", True)

        describe "pop" $
            it "pop something alias" $
              output arrayDB ["array", "-p"] `shouldBe` ("1", True)

        describe "pop" $
            it "pop invalid, on array" $
              output keyDB ["--pop"] `shouldBe`
                (popTypeError, False)


        -- assign
        describe "assign" $
            it "assign something" $ do
                cleanup
                let cx = getServerlessContext tmpFile
                    value = "sauce" :: String
                set cx ["apple"] value

                result <- get cx ["apple"]
                result `shouldBe` Just value

        describe "assign" $
            it "assign no change" $
              output keyDB ["apple", "=", "sauce"] `shouldBe` ("", False)


-- example databases

keyDB :: Value
keyDB = Object $ HM.fromList
    [ ("apple", "sauce")
    , ("blue", "berry")
    ]

arrayDB :: Value
arrayDB = Object $ HM.fromList [("array", array)]
    where
        array = Array $ V.fromList ["1", "2", "3"]


-- helpers

run :: Value -> Query -> (Text, Bool, Value)
run = runAction

output :: Value -> Query -> (Text, Bool)
output db query = (text, changed)
    where
        (text, changed, _) = run db query

result :: Value -> Query -> Text
result db query = text
    where
        (text, _, _) = run db query

getChanged :: Value -> Query -> Bool
getChanged db query = changed
    where
        (_, changed, _) = run db query

value :: Value -> Query -> Value
value db query = value
    where
        (_, _, value) = run db query

-- errors

keyTypeError :: Text
keyTypeError = "error: cannot retrieve keys for non-dict"

popTypeError :: Text
popTypeError = "error: this type does not support pop"


-- persistence

tmpFile :: FilePath
tmpFile = "test/apocrypha.test"

cleanup :: IO ()
cleanup = do
      exists <- doesFileExist tmpFile
      when exists $ removeFile tmpFile
