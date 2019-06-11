
module Core where

import           Clips
import qualified Data.Text  as T
import           Test.Hspec

spec :: Spec
spec = do
    let evalClips = withClipsT . eval
    describe "it works with multifield values" $ do
        runIO $ putStrLn "asdoiasd"
        it "(create$ 1 2 3 4)" $ do
            evalClips "(create$ 1 2 3 4)" `shouldReturn` ClipsMultifield (ClipsInteger <$> [1..4])
        it "(create$ a b c)" $ do
            evalClips "(create$ a b c)" `shouldReturn` ClipsMultifield (ClipsSymbol . T.pack <$> ["a", "b", "c"])
        it "works with void" $
            evalClips "(print)" `shouldReturn` ClipsVoid
        it "works with integers" $
            evalClips "1" `shouldReturn` ClipsInteger 1
        it "works with strings" $
            evalClips "\"str\"" `shouldReturn` ClipsString (T.pack "str")
        it "works with symbols" $
            evalClips "str" `shouldReturn` ClipsSymbol (T.pack "str")
        it "works with facts" $ do
            let isFact (ClipsFact _) = True
                isFact _             = False
            (isFact <$> evalClips "(assert (duck 1 2))") `shouldReturn` True
