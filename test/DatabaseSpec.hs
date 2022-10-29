{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec (spec) where

import           Apocrypha.Client
import           Apocrypha.Database
import           Apocrypha.Internal.Database

import           Data.Aeson
import qualified Data.Aeson.KeyMap           as HM
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Test.Hspec


contextSpec :: Spec
contextSpec = do
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

keysSpec :: Spec
keysSpec =
    describe "keys" $ do
        it "keys nothing" $
          output keyDB ["non-existant"] `shouldBe` ("", False)

        it "keys something" $
          output keyDB ["--keys"] `shouldBe` ("apple\nblue", False)

        it "keys something alias" $
          output keyDB ["-k"] `shouldBe` ("apple\nblue", False)

        it "keys invalid, on value" $
          output keyDB ["apple", "--keys"] `shouldBe`
            (keyTypeError, False)

        it "keys invalid, on array" $
          output arrayDB ["array", "--keys"] `shouldBe`
            (keyTypeError, False)

popSpec :: Spec
popSpec =
    describe "pop" $ do
        it "pop nothing" $
          output arrayDB ["non-existant", "--pop"] `shouldBe` ("", False)

        it "pop something" $
          output arrayDB ["array", "--pop"] `shouldBe` ("1", True)

        it "pop something alias" $
          output arrayDB ["array", "-p"] `shouldBe` ("1", True)

        it "pop invalid, on array" $
          output keyDB ["--pop"] `shouldBe`
            (popTypeError, False)

        it "simple" $
            runner ["letters = a b c", "letters -p", "letters -p"]
                `shouldBe` ["", "a", "b"]

assignSpec :: Spec
assignSpec =
    describe "assign" $ do
        it "simple" $ do
            runner ["a = b", "a"] `shouldBe` ["", "b"]
            runner ["a = b c", "a"] `shouldBe` ["", "b\nc"]

        it "nested" $ do
            runner ["a b = c", "a b"] `shouldBe` ["", "c"]
            runner ["a b = c", "a"] `shouldBe` ["", "{\n    \"b\": \"c\"\n}"]

        it "no change" $
          output keyDB ["apple", "=", "sauce"] `shouldBe` ("", False)

jsonSpec :: Spec
jsonSpec =
    describe "json" $ do
        it "string" $
            scenario
                [ ("a --set \"b\"", "")
                , ("a", "b")
                , ("a --edit", "\"b\"")
                ]

        it "number" $
            scenario
                [ ("a --set 1", "")
                , ("a", "1")
                , ("a --edit", "1")
                ]

        it "number array" $
            scenario
                [ ("a --set [1,2,3]", "")
                , ("a", "1\n2\n3")
                , ("a --edit", "[\n    1,\n    2,\n    3\n]")
                ]

dereferenceSpec :: Spec
dereferenceSpec =
    describe "dereference" $ do
        it "nothing" $ do
            cx <- getContext ServerMemory
            get cx ["apple"] `shouldReturn` (Nothing :: Maybe String)

        it "basic" $
            scenario
                [ ("letters = a b c", "")
                , ("a = 1", "")
                , ("b = 2", "")
                , ("c = 3", "")
                , ("!letters", "1\n2\n3")
                -- you already know the context, it's the content
                , ("-c !letters", "1\n2\n3")
                ]

        it "nested" $
            scenario
                [ ("letters = a b c", "")
                , ("a sub = 1", "")
                , ("b sub = 2", "")
                , ("c sub = 3", "")
                , ("!letters sub", "1\n2\n3")
                , ("-c !letters sub", "a = 1\nb = 2\nc = 3")
                ]

        it "deeply nested" $
            scenario
                [ ("letters = a b c", "")
                , ("a sub sib seb = 1", "")
                , ("b sub sib seb = 2", "")
                , ("c sub sib seb = 3", "")
                , ("!letters sub sib seb", "1\n2\n3")
                , ( "-c !letters sub sib seb"
                  , "a = sub = sib = 1\nb = sub = sib = 2\nc = sub = sib = 3")
                ]

{- unsupported
        it "double" $
            scenario
                [ ("letters = a b c", "")
                , ("a = 1", "")
                , ("b = 2", "")
                , ("c = 3", "")
                , ("!letters", "1\n2\n3")
                , ("1 = x", "")
                , ("2 = y", "")
                , ("3 = z", "")
                , ("!!letters", "x\ny\nz")
                ]
-}

spec :: Spec
spec = do
    contextSpec
    keysSpec
    popSpec
    assignSpec
    jsonSpec
    dereferenceSpec

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

scenario :: [(Text, Text)] -> Expectation
scenario ss = runner qs `shouldBe` os
    where
        (qs, os) = unzip ss

runner :: [Text] -> [Text]
runner = go db . map T.words
    where
        db = Object $ HM.fromList []

        go _ [] = []
        go d (q:qs) = output : go newDb qs
            where
                (output, _, newDb) = runAction d q

run :: Value -> Query -> (Text, Bool, Value)
run = runAction

output :: Value -> Query -> (Text, Bool)
output db query = (text, changed)
    where
        (text, changed, _) = run db query


-- errors

keyTypeError :: Text
keyTypeError = "error: cannot retrieve keys for non-dict"

popTypeError :: Text
popTypeError = "error: this type does not support pop"
