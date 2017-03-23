-- mod12PA.hs: Read points from stdin and generate an SVG image on stdout showing
-- the points, the axes, and the convex hull enclosing the points.
-- The input file must have points written as Haskell pairs (Float,Float).
-- Miles Greatwood

import Points
import Data.List.Split

-----------------------------------------------------------------------------
-- The Pure Part: Generate SVG strings that describe the canvas, viewbox, and
-- all the lines and points to display, as well as some input processing.

-- Make an SVG file header given the min X, max X, min Y, and max Y
-- coordinates of all the points in a graph.
svgHeader :: Float -> Float -> Float -> Float -> String
svgHeader minX maxX minY maxY =
   "<svg xmlns='http://www.w3.org/2000/svg' width=\"600\" height=\"600\"" ++
   " preserveAspectRatio=\"xMinYMid meet\"" ++
   " viewbox=\"" ++ (show minX) ++ " "
                 ++ (show (negate maxY)) ++ " "
                 ++ (show width) ++ " "
                 ++ (show height) ++ "\">"
   where
      width  = abs (maxX - minX)
      height = abs (maxY - minY)

-- Generate the SVG line section header give the min X, max X, min Y, and
-- max Y coordinates of all the points in the graph and the desired line
-- thickness.
svgLineSection :: Float -> Float -> Float -> Float -> Float -> String
svgLineSection minX maxX minY maxY t =
   "<g stroke=\"black\" stroke-width=\"" ++ (show t)
      ++ "\" transform=\"scale(1 -1)\">"
      ++ edgeToSVG ((minX,0),(maxX,0))     -- the x axis
      ++ edgeToSVG ((0,minY),(0,maxY))     -- the y axis

-- Generate the SVG line section footer and point section header.
svgPointSection :: String
svgPointSection =
   "</g>\n<g stroke=\"red\" fill=\"red\" stroke-width=\"0\" transform=\"scale(1 -1)\">"

-- Generate the SVG point section footer and file footer.
svgFooter :: String
svgFooter = "</g>\n</svg>"

-- Convert an Edge to an SVG line with those coordinates.
edgeToSVG :: Edge Float -> String
edgeToSVG ((e1x,e1y),(e2x,e2y)) =
   ("   <line x1=\"" ++ (show e1x) ++ "\" y1=\"" ++ (show e1y) ++
          "\" x2=\"" ++ (show e2x) ++ "\" y2=\"" ++ (show e2y) ++ "\"/>")

-- Convert a Point to an SVG circle at that coordinate with a radius of r.
pointToSVG :: Float -> Point Float -> String
pointToSVG r (x,y) =
  "   <circle cx=\"" ++ (show x) ++ "\" cy=\"" ++ (show y) ++ "\" r=\"" ++(show r)++ "\"/>"

-- Convert a String with points separated by spaces to a list of Points.
--SplitOn turns the strings into a list of strings
--The read function turns those elements in the list of Strings and turns them into Float,Float
toPoints :: String -> [(Float,Float)]
toPoints str = map (read::String->(Float,Float))(splitOn " " str)
-----------------------------------------------------------------------------
-- Impure part: Read the data from stdin, find the convex hull, and write the
-- SVG to stdout.

-- Take a list of strings and write them to stdout, one per line.
printList :: [String] -> IO ()
printList line = mapM_ putStrLn line
--Printing these lines using the head of the list seems to print ALL of the strings at once.
--I want to print each string on it's own line
--mapM and mapM_ are monadic versions of map
--Monadic means it is a "composable computation"
--"The essence of monad is thus separation of composition timeline from the composed computation's execution timeline"
--The underscore one doesn't save/reutrn the results of the putStrLn, it just runs
--Here's the type signature of mapM_ mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
--I got this from http://stackoverflow.com/questions/932639/haskell-cant-use-map-putstrln

-- The main program: read points from stdin, write an SVG file to stdout.
main :: IO ()
main = do
 input <-  getContents  --This grabs the input form stdIn.
 let listOfPoints = toPoints input --Contains a list of Points. It must be a let constant to work.
 putStrLn $ show $ head listOfPoints -- This prints the first point
 --From here on I have to focus on making the SVG file

--My Tests written to work with my functions
testToPoints :: Bool
testToPoints =
  let actual = toPoints "(1.1,1.2) (2.1,2.2) (3.1,3.2)"
  in  actual == [(1.1, 1.2),(2.1, 2.2),(3.1, 3.2)]

testPrintList = printList ["String1", "String2", "What", "Is", "Up"]

myTests = testToPoints
