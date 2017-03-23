--We can import module files

module MyModule where
myLength :: [a] -> Int
myLength arr = length arr

myHead :: Ord a => [a] -> a
myHead arr = head arr
