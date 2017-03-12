-- mod10PA.hs -- Module 10 Haskell functions
-- Miles Greatwood

import Data.Function (on)
import Data.List (sortBy)
import Data.List (genericTake)


-- A list of all factors of n.
--The second version of this is a recursive function.
factors :: Integral a => a -> [a]
factors n = [ allInts | allInts <- [1..n], n `mod` allInts  == 0]
--factors n = factoring n n

--Helper function for returning factors RECURSIVLEY
factoring :: Integral a => a -> a -> [a]
factoring n num
  | n <=0 = []
  | num `mod` n == 0 = (factoring (n-1) num) ++ [n]
  | otherwise = factoring (n-1) num
--  | num `mod` n /= 0 = (factoring (n-1) num)

-- True iff n is prime.
isPrime :: Integral a => a -> Bool
isPrime n = length (factors n) == 2

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = [ myFactors | myFactors <- factors n, isPrime myFactors ]

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [primes | primes <-[2..n] ,isPrime primes]

-- True iff n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect n
  | n < 1 = False
  |otherwise = ((sum (factors n)) == 2 * n)

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n
  | n < 6 = []
  | otherwise = [p | p<-[6..n], isPerfect p ]

get3 :: (a,b,c) -> c
get3 (_,_,c) = c

-- A list of pythagorean triples.
-- A triple (x,y,z) is pythagorean if x^2+y^s = z^2.
--In this result, x <= y < z.
--I had to write a cutsom function to get the third part of the tuple for ordering
pythagoreans :: Integral a => a -> [(a,a,a)]
pythagoreans n
  | n < 5 = []
  | otherwise =  sortBy (compare `on` get3) [(x,y,z)| x <- [3..n], y <- [4..n], z <- [5..n], x^2 + y^2 == z^2, x <= y, y < z]
--I need a way to reorder the list by the third element

-- The next prime greater than n.
nextPrime :: Integral a => a -> a
nextPrime n
 | n < 2 = 2
 | otherwise = findNextPrime (n+1)

findNextPrime :: Integral a => a -> a
findNextPrime n
 | isPrime n = n
 | otherwise = findNextPrime (n+1)

findNextPrimeEndOfList :: Integral a => [a] -> a
findNextPrimeEndOfList x = findNextPrime (last x)

-- A list of the first n primes.
generatePrimes :: Integral a => a -> [a]
generatePrimes n
 | n < 1 = []
 | otherwise =  genericTake n [ p | p<-2:[3,5..], isPrime p]
--I wanted to write something recursive like...
--- |otherwise = generatePrimes(n-1) ++ findNextPrime (last generatePrimes(n-1))
--Ask Bowers what the correct syntax for this recursive call would be

-- | otherwise =  generatePrimes (n(-1)) ++ findNextPrimeEndOfList (generatePrimes (n-1))

-- Helper function: a list of the first n primes larger than p.
generateNprimesAfter :: Integral a => a -> a -> [a]
generateNprimesAfter n p
 | n < 1 = []
 | otherwise =  genericTake n [ m | m<-[(p+2),(p+4)..], isPrime m]
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

testNextPrime =
   nextPrime (-1) == 2 &&
   nextPrime 5 == 7 &&
   nextPrime 200 == 211

testGeneratePrimes =
   generatePrimes (-3) == [] &&
   generatePrimes 0 == [] &&
   generatePrimes 1 == [2] &&
   generatePrimes 15 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

testPythagoreans =
  pythagoreans (-1) == [] &&
  pythagoreans 5 == [(3,4,5)] &&
  pythagoreans 20 == [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]


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
