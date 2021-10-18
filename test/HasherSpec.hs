{-# LANGUAGE OverloadedStrings #-}

module HasherSpec (spec) where

import           Apocrypha.Server.Plugin.Hasher

import           Data.Aeson
import qualified Data.HashMap.Strict         as HM
import           Test.Hspec


spec :: Spec
spec = do
        describe "values" $ do
            it "round trip" $ do
                let entry = Entry zeroTime knownValue
                valueToEntry (entryToValue entry) `shouldBe` Just entry

            it "corrupt" $ do
                valueToEntry Null `shouldBe` Nothing
                valueToEntry junkEntry `shouldBe` Nothing

        describe "io" $ do
            it "getHash" $ do
                getHash "/not/a/real/path" `shouldReturn` Nothing
                getHash path `shouldNotReturn` Nothing

            it "getModTime" $ do
                getModTime "/not/a/real/path" `shouldReturn` Nothing
                getModTime path `shouldNotReturn` Nothing

        describe "hash" $ do
            it "cache miss" $ do
                (t, changed, _) <- hash emptyDB path

                t `shouldNotBe` ""
                changed `shouldBe` True

            it "cache hit stale" $ do
                (t, changed, _) <- hash filledDB path

                t `shouldNotBe` knownValue
                changed `shouldBe` True

            it "cache hit fresh" $ do
                (t1, c1, db1) <- hash filledDB path
                c1 `shouldBe` True

                (t2, c2, _) <- hash db1 path
                t2 `shouldBe` t1
                c2 `shouldBe` False

                (t3, c3, _) <- hash db1 path
                t3 `shouldBe` t1
                c3 `shouldBe` False

    where
        path = "README.md"
        emptyDB = Object $ HM.fromList []

        knownValue = "expected"
        filledDB = update Null path (Entry zeroTime knownValue)

        zeroTime = epochToUtc 0

        junkEntry = Object $ HM.fromList [("s", Null), ("h", Null)]
