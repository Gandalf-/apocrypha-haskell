module ProtocolSpec (spec) where

import           Test.Hspec


spec :: Spec
spec = do
        describe "something" $
          it "does a thing" $
            1 `shouldBe` 1

        describe "something else" $
          it "does a thing" $
            2 `shouldBe` 2
