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

    describe "factorization in primes" $ do
        it "corner cases" $ do
	  factors 0 `shouldBe` []
	  factors 1 `shouldBe` []

        it "should decompose a prime" $ do
	  factors 2 `shouldBe` [2]

        it "should decompose a non-prime" $ do
	  factors 6 `shouldBe` [2, 3]

        it "should decompose a non-prime with repeated factors" $ do
	  factors 8 `shouldBe` [2, 2, 2]


sumDigits :: Integer -> Integer
sumDigits n = (n `mod` 10) + if n >= 10 then (sumDigits (n `div` 10)) else 0

isPrime n = length [i|i<-[2..n], n `mod` i == 0] == 1

factors :: Integer -> [Integer]
factors n = reverse $ factors' n []

factors' :: Integer -> [Integer] -> [Integer]
factors' 0 primes = []
factors' 1 primes = primes 
factors' n current = factors' (n `div` i) (i:current)
    where i = head [i|i<-[head' current 2..n], n `mod` i == 0, isPrime i]

head' :: [a] -> a -> a
head' [] x = x
head' (h:t) _ = h
