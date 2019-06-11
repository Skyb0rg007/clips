
import qualified Core
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Core" Core.spec
