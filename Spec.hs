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

    describe "smith numbers" $ do
        it "should detect the corner cases" $ do
	  isSmith 0 `shouldBe` False
	  isSmith 1 `shouldBe` False

        it "should detect positively the prime numbers" $ do
	  isSmith 2 `shouldBe` True
	  isSmith 3 `shouldBe` True
	  isSmith 5 `shouldBe` True

	it "should detect the smallest, composite Smith number" $ do
	  isSmith 4 `shouldBe` True

	it "should detect non-Smith numbers" $ do
	  isSmith 6 `shouldBe` False

	it "should detect all OEIS Smith numbers" $ do
	  filter (\x->x == False) (map (\x -> isSmith x) [4, 22, 27, 58, 85, 94, 121, 166, 202, 265, 274, 319, 346, 355, 378, 382, 391, 438, 454, 483, 517, 526, 535, 562, 576, 588, 627, 634, 636, 645, 648, 654, 663, 666, 690, 706, 728, 729, 762, 778, 825, 852, 861, 895, 913, 915, 922, 958, 985, 1086, 1111, 1165]) `shouldBe` []


sumDigits :: Integer -> Integer
sumDigits 0 = 0
sumDigits n = (n `mod` 10) + (sumDigits (n `div` 10))

divides :: Integer -> Integer -> Bool
divides divisor n = n `mod` divisor == 0

isPrime n = length [i|i<-[2..n], i `divides` n] == 1

factors :: Integer -> [Integer]
factors n = reverse $ factors' n []

factors' :: Integer -> [Integer] -> [Integer]
factors' 0 primes = []
factors' 1 primes = primes 
factors' n current = factors' (n `div` i) (i:current)
    where i = head [i|i<-[headOr current 2..n], i `divides` n, isPrime i]

headOr :: [a] -> a -> a
headOr [] default' = default'
headOr (h:t) _ = h

isSmith :: Integer -> Bool
isSmith n = if (null . factors) n
            then False
            else (sumDigits n) == foldl1 (+) (map sumDigits (factors n))
