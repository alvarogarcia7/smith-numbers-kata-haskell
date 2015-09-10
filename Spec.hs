import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

    describe "sum of digits" $ do
        it "should sum one digit" $ do
	  sumDigits 1 `shouldBe` 1

        it "should sum two digits" $ do
	  sumDigits 12 `shouldBe` 3

        it "should sum three digits" $ do
	  sumDigits 123 `shouldBe` 6

        it "should sum four digits" $ do
	  sumDigits 1234 `shouldBe` 10


sumDigits :: Int -> Int
sumDigits n = (n `mod` 10) + if n >= 10 then (sumDigits (n `div` 10)) else 0
