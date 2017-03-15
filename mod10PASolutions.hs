-- mod10PA.hs -- Module 10 Haskell functions
-- Prof. John C. Bowers

-- A list of all factors of n.
factors :: Integral a => a -> [a]
factors n = [x | x<-[1..n], n `mod` x == 0]

-- True iff n is prime.
isPrime :: Integral a => a -> Bool
isPrime n = (factors n) == [1, n]

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = filter isPrime (factors n)

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n = filter isPrime [2..n]

-- True iff n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect 0 = False
isPerfect n = (2*n) == sum (factors n)

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = filter isPerfect [6..n]

-- A list of pythagorean triples.
-- A triple (x,y,z) is pythagorean if x^2+y^s = z^2. In this result, x <= y < z.
pythagoreans :: Integral a => a -> [(a,a,a)]
pythagoreans n = [(x,y,z) | z<-[5..n],x<-[3..n],y<-[4..n],x*x+y*y==z*z, x<y]

-- The next prime greater than n.
nextPrime :: Integral a => a -> a
nextPrime n
  | isPrime (n+1) = (n+1)
  | otherwise = nextPrime (n+1)

-- A list of the first n primes.
generatePrimes :: Integral a => a -> [a]
generatePrimes n
  | n <= 0 = []
  | otherwise = generateNprimesAfter n 1

-- Helper function: a list of the first n primes larger than p.
generateNprimesAfter :: Integral a => a -> a -> [a]
generateNprimesAfter 0 _ = []
generateNprimesAfter n a = p:(generateNprimesAfter (n-1) p) where p = nextPrime a

--------------------------------------------------------------
-- Tests: Use this code as is to make sure your PA is correct.

testFactors =
   (factors (-8)) == [] &&
   (factors 0) == [] &&
   (factors 1) == [1] &&
   (factors 2) == [1,2] &&
   (factors 3) == [1,3] &&
   (factors 4) == [1,2,4] &&
   (factors 5) == [1,5] &&
   (factors 24) == [1,2,3,4,6,8,12,24]

testIsPrime =
   not (isPrime (-5)) &&
   not (isPrime 0) &&
   not (isPrime 1) &&
       (isPrime 2) &&
       (isPrime 3) &&
   not (isPrime 4) &&
       (isPrime 5) &&
       (isPrime 17389)

testPrimeFactors =
   primeFactors (-3) == [] &&
   primeFactors 0 == [] &&
   primeFactors 1 == [] &&
   primeFactors 5 == [5] &&
   primeFactors 24 == [2,3] &&
   primeFactors 40320 == [2,3,5,7]

testPrimesUpTo =
   primesUpTo (-2) == [] &&
   primesUpTo 2 == [2] &&
   primesUpTo 5 == [2,3,5] &&
   primesUpTo 8 == [2,3,5,7] &&
   primesUpTo 43 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43]

testIsPerfect =
   not (isPerfect (-3)) &&
   not (isPerfect 0) &&
   not (isPerfect 1) &&
   not (isPerfect 2) &&
        isPerfect 6 &&
        isPerfect 28 &&
   not (isPerfect 426)

testPerfectUpTo =
   perfectUpTo (-7) == [] &&
   perfectUpTo 27 == [6] &&
   perfectUpTo 28 == [6,28]

testPythagoreans =
   pythagoreans (-1) == [] &&
   pythagoreans 5 == [(3,4,5)] &&
   pythagoreans 20 == [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]

testNextPrime =
   nextPrime (-1) == 2 &&
   nextPrime 5 == 7 &&
   nextPrime 200 == 211

testGeneratePrimes =
   generatePrimes (-3) == [] &&
   generatePrimes 0 == [] &&
   generatePrimes 1 == [2] &&
   generatePrimes 15 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

test =
   testFactors &&
   testIsPrime &&
   testPrimeFactors &&
   testPrimesUpTo &&
   testIsPerfect &&
   testPerfectUpTo &&
   testPythagoreans &&
   testNextPrime &&
   testGeneratePrimes
