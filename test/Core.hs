
module Core where

import Test.Hspec
import Clips.Raw.Types

spec :: Spec
spec = do
    describe "Works with environment functions" $ do
        it "Creates and destroys environments" $
            (createEnvironment >>= destroyEnvironment) `shouldReturn` ()

        it "asd" $ 2 `shouldBe` (1 :: Int)
    describe "another" $ it "asdf" $ "hello" `shouldBe` "qqq"
