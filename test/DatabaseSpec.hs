{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import           Apocrypha.Database
import           Apocrypha.Internal.Database

import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import           Data.Text                   (Text)
import qualified Data.Vector                 as V

import           Test.Hspec


spec :: Spec
spec = do
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
              output keyDB ["non-existant"] `shouldBe` ("\n", False)

        describe "get" $
            it "get something" $
              output keyDB ["apple"] `shouldBe` ("sauce\n", False)


        -- keys
        describe "keys" $
            it "keys nothing" $
              output keyDB ["non-existant"] `shouldBe` ("\n", False)

        describe "keys" $
            it "keys something" $
              output keyDB ["--keys"] `shouldBe` ("apple\nblue\n", False)

        describe "keys" $
            it "keys invalid, on value" $
              output keyDB ["apple", "--keys"] `shouldBe`
                (keyTypeError, False)

        describe "keys" $
            it "keys invalid, on array" $
              output arrayDB ["array", "--keys"] `shouldBe`
                (keyTypeError, False)


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
run db query = runAction db query

output :: Value -> Query -> (Text, Bool)
output db query = (text, changed)
    where
        (text, changed, _) = run db query

result :: Value -> Query -> Text
result db query = text
    where
        (text, _, _) = run db query

changed :: Value -> Query -> Bool
changed db query = changed
    where
        (_, changed, _) = run db query

value :: Value -> Query -> Value
value db query = value
    where
        (_, _, value) = run db query

-- errors

keyTypeError = "error: cannot retrieve keys for non-dict\n"
