--Miles Greatwood
sumOfFirstN :: Integer -> Integer
sumOfFirstN n = n * (n+1) `div` 2

points :: Int -> Int
points 1 = 10
points 2 = 8
points 3 = 6
points 4 = 5
points 5 = 4
points 6 = 3
points 7 = 2
points 8 = 1
points _ = 0

points2 :: Int -> Int
points2 n
 | (9 < n) || (n<1) =0
 | (n <= 3) = 12 - 2*n
 | otherwise = 9 - n


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

--Since we don't have variables we need recursion
--Writing this using variables and iteration takes forever
--This calculates Fibonacci numbers with recursion
--It needs parameters to store counters and accumulaters

ffib n = fibAccum 1 1 n
fibAccum e0 e1 k
 | (k == 0) = e0
 | (k == 1) = e1
 | otherwise = fibAccum e1 (e0+e1) (k-1)

--The Eq a => in type signature means a is any type whose values
--can be compared for equality. a is a subclass of comparable.
--This works for ANY types as long...
--a is a comparable type
--a and a list of [a] values are parameters
--the function must return an integer

--For example, indexOf 'e' "fast times" is 8.
indexOf :: Eq a => a -> [a] -> Integer
indexOf x [] = 3
indexOf letter word
-- | (letter == (head word)) = 1
 | otherwise =5

--I need to write it

--HOW DO I WRITE THE indexOf function
--I thought it was something like
--indexOf 'char' "string" = number
--Where do i get the parameter names from?
--Before they were in the function definition
