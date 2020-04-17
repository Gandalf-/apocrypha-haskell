module OptionsSpec (spec) where

import           Apocrypha.Internal.Options
import           Apocrypha.Options

import           Data.List                  (isSubsequenceOf)
import           Test.Hspec


spec :: Spec
spec = do
        -- getOptions
        describe "get options" $
          it "invalid" $
            parseOptions path 9999 ["junk", "--headless"] `shouldBe` Nothing

        describe "get options" $
          it "headless" $
            parseOptions path 9999 ["--headless"] `shouldBe` (Just $ Left sample)


        -- parse
        describe "parse" $
          it "db path" $
            parse ["--database", "a"] `shouldBe` [OtherDatabase "a"]

        describe "parse" $
          it "db path invalid" $
            parse ["--database"] `shouldBe` [InvalidOption]

        describe "parse" $
          it "headless" $
            parse ["--headless"] `shouldBe` [NoLogging]

        describe "parse" $
          it "tcp port" $
            parse ["--tcp-port", "99"] `shouldBe` [OtherTCPPort "99"]


        -- chooseDB
        describe "choose database" $
          it "default" $
            chooseDB [NoLogging, NoState] "a" `shouldBe` "a"

        describe "choose database" $
          it "not found" $
            chooseDB [] "a" `shouldBe` "a"

        describe "choose database" $
          it "alternate" $
            chooseDB [NoLogging, OtherDatabase "b"] "a" `shouldBe` "b"


        -- choosePort
        describe "choose port" $
          it "default" $
            choosePort [NoLogging, NoState] port `shouldBe` port

        describe "choose port" $
          it "alt" $
            choosePort [NoLogging, OtherTCPPort "8888"] port `shouldBe` 8888


        -- parseProxy
        describe "choose port" $
          it "default" $
            parseProxy [NoLogging, NoState] port `shouldBe` Nothing

        describe "choose port" $
          it "host and port" $ do
            let e = Just ("example.com", 8888)
            parseProxy [NoLogging, Proxy "example.com:8888"] port `shouldBe` e

        describe "choose port" $
          it "host and default port" $ do
            let e = Just ("example.com", port)
            parseProxy [NoLogging, Proxy "example.com"] port `shouldBe` e


        -- usage
        describe "usage" $
          it "isn't missing anything" $
            all (`isSubsequenceOf` usage) options `shouldBe` True


    where
        sample = DatabaseOptions False 9999 True True True path
        options = words "headless no-cache stateless no-unix tcp-port database"
        path = "/default/db/path"
        port = 9999
