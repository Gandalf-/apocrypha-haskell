module OptionsSpec (spec) where

import           Apocrypha.Internal.Options
import           Apocrypha.Options
import           Test.Hspec


spec :: Spec
spec = do
        -- getOptions
        describe "get options" $
          it "invalid" $
            getOptions path ["junk", "--headless"] `shouldBe` Nothing

        describe "get options" $
          it "headless" $
            getOptions path ["--headless"] `shouldBe` Just headless


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
    where
        headless = Options False True True path True
        path = "/default/db/path"
