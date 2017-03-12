--Lab11 Haskell
--Miles Greatwood

--Defining Types
--type T = <def>
type MyStrings = [Char]

--Haskell really uses currying for function types
--listOf4 is really a -> (a -> (a -> (a -> [a])))
--The first functio takes a as infput and returns a function as result
--Always takes only one parameter and deals with one at a time
listOf4 :: a ->  a -> a -> a -> [a]
listOf4 a b c d = [a,b,c,d]

--This tacks an 8 on the front of a list with three elements
listOf4With8 a b c = listOf4 8 a b c

--Functions can be operations to be applied
--The tyoe is assumed to be Integer -> Integer
--Functions that don't specify parameters are  point-free defnitions
double = (2*)

--Functions can be returned values or arguments
doubleList [] = []
doubleList (n:ns) = (double n):(doubleList ns)

--Applies some operation ot every function in a list
--The first param (a -> a) is a function
--Second param is a list and
--It returns a list
applyToList :: (a -> a) -> [a] -> [a]
applyToList _ [] = []
applyToList f (n:ns) = (f n):(applyToList f ns)

--Examples of applying funcitons to lists
examplesListFunctions1 =
  applyToList (2*) [1..10]
examplesListFunctions2 =
  applyToList double [1..10]

--Use :t fo get the type of functions
--applyToList already exists as the funcito map
examplesListFunctions3 =
  map odd[1..10]

lengthsOfStrings = map length ["Word1", "MilesWasHere", "What's UP"]

--foldr and foldl are Higher Order Functions
--They let you apply functions to a list and ALSO have an accumulator value
--r goes right and l goes left
--Parameters
  --A binary function (Integer -> Integer)
  --An Accumulator value 0
  --A list of input
-- (+) is a binary function of type (+) :: Num a => a -> a -> a
foldrExample = foldr (+) 0 [0..100]
--foldl1 foldr1 don't have a starting accumulator
--They just use the firts or last element
foldr1Example = foldl1 (+) [0..100]
foldr1ExampleBooleans = foldr1 (&&) [True, True, False, True, True]

--Create your own Binary Functions
maxString :: [Char] -> [Char] -> [Char]
maxString a b = if (length a > length b) then a else b
--Use your custom binary function in a fold function
findMaxStringInList = foldr1 (maxString) ["Word1", "asdfasdfasahsdgfliasgdkfjhaslkjdhfklashdlkfjhaskjhdflkhasliugydiua" ,"MilesWasHere", "What's UP", "alfkdjhflasdgflkhasdkjfhksd"]

--The let construct allows for temporary definition of name bindings
maximumString :: [[Char]] -> [Char]
maximumString list =
 let
  localMaxString s t
   | (length s) < (length t) = t
   | otherwise = s
 in foldl (localMaxString) "" list
--The expression following in can use the local function maxString

--Alternate versio using where clause instead of let in
maximumString' :: [[Char]] -> [Char]
maximumString' = foldl maxString ""
 where
 maxString s t
  | (length s) < (length t) = t
  | otherwise = s

--Indentation rules
--1) expression code is always further indented
--2) Group together similar expressions into the same level

--Filter functions
--filter :: (a -> Bool) -> [a] -> [a]
filterExample1 = filter odd [1..100]

--Writing Quicksort
partitionLess :: Ord a => a -> [a] -> [a]
partitionLess v l = filter (<v) l

partitionMore :: Ord a => a -> [a] -> [a]
partitionMore v l = filter (>=v) l

myQuickSort :: Ord a => [a] -> [a]
myQuickSort (x:xs) = smallerSorted ++ [x] ++ largerSorted
  where
  smallerSorted = myQuickSort (partitionLess x xs)
  largerSorted = myQuickSort (partitionMore x xs)
myQuickSort [] = []

myQuickSort2 :: Ord a => [a] -> [a]
myQuickSort2 (x:xs) = smallerSorted ++ [x] ++ largerSorted
  where
  smallerSorted = myQuickSort [a | a <- xs, a <=x]
  largerSorted = myQuickSort [a | a <- xs, a > x]
myQuickSort2 [] = [] --This is needed for once the array is empty

 --Making functions right associative
leftAssociativeFunctions = odd (mod 8 (div 9 3))
rightAssociativeFunctions = odd $ mod 8 $ div 9 3
--The $ has lower precedence than function application (left associative) SO it effectivley makes statements right associative
--First div gets executed then mod then odd
-- odd applied to mod applied to div
