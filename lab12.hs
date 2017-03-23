--lab12.hs
--Miles Greatwood

--Here we are importing functions from another file in the same directory
import MyModule

--Read -- converting strings to types
--These first two work fine
readFucntion = read "8" + 5
readFucntion2 = read "[1,2,3]" :: [Int]
--readFucntion3 = read "8" -- This will not work because it can't tell what type it is

--Show does the opposite of read
showFunc1 = "Pair " ++ show (9,2)

--Actions are impure functions meaning they return IO
--Pure functions don't use IO so they are easier to predict answers
--All programs start with the action main :: IO()

--runhaskell nameOfFile compiles and runs the file
--main = putStrLn "Hello From My Main Defined Action"

--This do block allows us to order things in Haskell (that's new)
--The putStrLn puts a string into IO and <- takes it out and assings it to name
--main = do
-- putStrLn "Your name?"
-- name <- getLine
-- putStrLn ("Hello " ++ name)

--The <- requires an IO on the right side. It's not an assignment
--Assignmeents can be done with let
--main = do
-- putStrLn "Your name?"
-- name <- getLine
-- let response = "Hello " ++ name
-- putStrLn response
--response is available until the end of the block


--getContents takes in all of the STDin content
main = do
 input <- getContents
 putStr input


 --Using return
 --return is different from Java and Ruby because return doesn't change control flow
 --return just puts a value into an IO cotext
