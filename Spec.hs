import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True
