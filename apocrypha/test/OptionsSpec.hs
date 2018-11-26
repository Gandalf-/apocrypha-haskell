module OptionsSpec (spec) where

import           Apocrypha.Options
import           Test.Hspec


spec :: Spec
spec = do
        describe "options" $
          it "enabled everything by default" $
            getOptions [] `shouldBe` Just defaultOptions

        describe "options" $
          it "headless" $
            getOptions ["--headless"] `shouldBe` Just headless

        describe "options" $
          it "stateless" $
            getOptions ["--stateless"] `shouldBe` Just stateless

        describe "options" $
          it "parse error" $
            getOptions ["invalid"] `shouldBe` Nothing

     where
         defaultOptions = Options True True True
         headless       = Options False True True
         stateless      = Options True True False
