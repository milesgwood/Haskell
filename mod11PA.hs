-- Mod11PA.hs -- CS 430 Spring 2017 Module 11 PA.
-- Miles Greatwood

------------------------------------------------------------------------------
-- Part 1: Closest pairs of point.
-- A closest pair of a set of points is a pair from the set whose distance is
-- no greater than the distance between any other pair of points from the set.

type Point a = (a,a)

--Find out why type errors occoured when a was Floating and b was Floating
--I was using sqrt ((x1-x2)^2 + (y1-y2)^2) but that wouldn't convert Floatings into Floating

-- Determine the true distance between two points.
distance :: (Floating a) => Point a -> Point a -> a
distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Determine the square of the distance between two points.
-- This is a faster alternative to finding the true distance in the closest pairs algorithm.
sqDistance :: (Floating a) => Point a -> Point a -> a
sqDistance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

type Pair a = (Point a, Point a)

-- Generate all pairs of points from a list of points.
-- Each point should appear exactly once in the result.
pairsFromPoints :: Floating a => [Point a] -> [Pair a]
pairsFromPoints n
  | length n < 2 = []
pairsFromPoints (n:ns) = pairsFromPoints ns ++ addToPairs n ns
--This makes each point the static point once and sends each point to the addToParis Funtion

--This function recursivley builds the list from the first point and the rest of the list
addToPairs :: Floating a => Point a -> [Point a] -> [Pair a]
addToPairs p1 (p2:rest)
  | length (p2:rest) == 0 = []
  | length (p2:rest) == 1 = [(p1,p2)]
  | otherwise = [(p1,p2)] ++ addToPairs p1 rest

--The xs is the end of the list. When there are only 2 elements xs has a length of 0
learnAboutXS = length [(2,-3), (8,3)]
learnAboutXS2 :: Floating a => [Point a] -> Int
learnAboutXS2 (x:xs) = length xs

-- Determine which of two pairs of points is the closer.
closerPair :: Ord a => Floating a => Pair a -> Pair a -> Pair a
closerPair (p1,p2) (q1,q2)
  | sqDistance p1 p2 < sqDistance q1 q2 = (p1,p2)
  | otherwise = (q1,q2)

-- Find the closest pair of points given a list of points (brute force).
closestPair :: Ord a => Floating a => [Point a] -> Pair a
closestPair points
  | length points < 2 = ((0,0), (0,0))
  | otherwise = bruteClosestPoints pairs (head pairs)
    where pairs = pairsFromPoints points

bruteClosestPoints :: Ord a => Floating a => [Pair a] -> Pair a -> Pair a
bruteClosestPoints (x:xs) pair
  | length xs == 0 = closerPair pair x
  | otherwise = bruteClosestPoints xs (closerPair pair x)

---------------------------------------------------------------------------
-- Part 2: Find the convex hull of a set of points.
-- A convex hull of a set of points is the subset of the points forming the
-- smallest convex polygon enclosing all of them.

type Edge a = (Point a, Point a)

-- Given an edge (that determines a line), say whether, as one moves along the
-- edge from its first to its second point, one must turn right (1) or left (-1)
-- to get to the point, or whether the point is on the line itself (0).
lineSide :: Floating a => Edge a -> Point a -> a
lineSide ((e1x,e1y),(e2x,e2y)) (px,py) = signum ((px-e1x)*(e2y-e1y) - (e2x-e1x)*(py-e1y))

isLineSide  :: Ord a => Floating a => Edge a -> Point a -> Bool
isLineSide ((e1x,e1y),(e2x,e2y)) (px,py)
  | (lineSide ((e1x,e1y),(e2x,e2y)) (px,py) >= 0) = True
  | otherwise = False

-- isHullEdge returns true just in case all points are on the same side of or
-- on the line determined by an edge.
isHullEdge :: Ord a => Floating a => [Point a] -> Edge a -> Bool
isHullEdge points edge
 | points == [] = True
 | length points >= 1 = recurseHullEdge points edge

recurseHullEdge :: Ord a => Floating a => [Point a] -> Edge a -> Bool
recurseHullEdge (x:xs) edge
  | length xs >=1 = (isLineSide edge x) && (recurseHullEdge xs edge)
  | otherwise = isLineSide edge x

-- Given a set of points, produce a list of all the edges in the convex hull for that set.
-- The result list has no duplicates.
--I am having trouble getting all of the Edges included in the list for some reason
convexHullEdges :: Ord a => Floating a => [Point a] -> [Edge a]
convexHullEdges points
  | length points < 2 = []
  | otherwise = checkAllEdges points (pairsFromPoints points)

--This take in all of the possible edges and our set of points
--If we are looking at a single edge, we check that edge
--Otherwise we check the first edge and recursivley call checkAllEdges on the rest of the edges
checkAllEdges :: Ord a => Floating a => [Point a] -> [Edge a] -> [Edge a]
checkAllEdges points (x:xs)
  | length xs == 0 = (checkEdge points x)
  | otherwise = (checkEdge points x) ++ (checkAllEdges points xs)

--This checks a single edge against our set of points
--If it is a hull edge we return the edge in a list
--Otherwise we return an empty set
checkEdge :: Ord a => Floating a => [Point a] -> Edge a -> [Edge a]
checkEdge points e
  | True == isHullEdge points e = [e]
  | False == isHullEdge points e = []

-- Given a set of points, produce a list of all the points forming the convex
-- hull for that set. The result has no duplicates.
convexHullPoints :: Ord a => Floating a => [Point a] -> [Point a]
convexHullPoints [] = []
convexHullPoints points = prunePoints (getPointsFromEdges (convexHullEdges points))

getPointsFromEdges :: Ord a => Floating a => [Edge a] -> [Point a]
getPointsFromEdges (n:ns)
  | length ns >= 1 = getPointsFromEdge n  ++ getPointsFromEdges ns
  | otherwise = getPointsFromEdge n
getPointsFromEdges [] = []

getPointsFromEdge :: Ord a => Floating a => Edge a -> [Point a]
getPointsFromEdge edge = [fst edge] ++ [snd edge]

--This removes dupliacte points from the list of ConvexHullPoints
prunePoints :: Ord a => Floating a => [Point a] -> [Point a]
prunePoints [] = []
prunePoints (x:xs)
 |True == checkForMatch x xs =  [] ++ prunePoints xs
 |False == checkForMatch x xs =  [x] ++ prunePoints xs

checkForMatch :: Ord a => Floating a => Point a -> [Point a] -> Bool
checkForMatch point [] = False
checkForMatch point (x:xs) = elem point xs

-- Tests -------------------------------------------------------------

tPoints       = [(3,5), (5,1), (6,3), (7,7), (9,4), (1,4), (4,4), (2,1), (8,2), (7,5), (3,8)]
tClosest      = ((3,5), (4,4))
tHullPoints   = [(1,4), (3,8), (7,7), (9,4), (8,2), (5,1), (2,1)]
tHullEdges    = [((5,1),(2,1)), ((5,1),(8,2)), ((7,7),(9,4)), ((7,7),(3,8)),
                 ((9,4),(8,2)), ((1,4),(2,1)), ((1,4),(3,8)),
                 ((2,1),(5,1)), ((8,2),(5,1)), ((9,4),(7,7)), ((3,8),(7,7)),
                 ((8,2),(9,4)), ((2,1),(1,4)), ((3,8),(1,4))]

testDistance =
   0 == distance (0,0) (0,0) &&
   5 == distance (1,1) (4,5) &&
   5 == distance (0,0) (negate 3, negate 4)

testCheckForMatch =
  False == checkForMatch (3,5) [(3,5)] &&
  True == checkForMatch (3,5) [(3,5), (3,5)]

testSqDistance =
   0  == sqDistance (0,0) (0,0) &&
   25 == sqDistance (1,1) (4,5) &&
   25 == sqDistance (0,0) (negate 3, negate 4)

testPairsFromPoints =
   [] == pairsFromPoints [] &&
   [] == pairsFromPoints [(2,-3)] &&
   [((2,-3),(8,3))] == pairsFromPoints [(2,-3), (8,3)] &&
   3 == length (pairsFromPoints [(2,-3), (8,3), (9,1)]) &&
   all (`elem` [((2,-3),(8,3)), ((2,-3),(9,1)), ((8,3),(9,1))]) (pairsFromPoints [(2,-3), (8,3), (9,1)])

testCloserPair =
   ((0,0),(1,1)) == closerPair ((0,0),(2,2)) ((0,0),(1,1)) &&
      (   ((0,0),(1,1)) == closerPair ((2,2),(1,1)) ((0,0),(1,1))
       || ((2,2),(1,1)) == closerPair ((2,2),(1,1)) ((0,0),(1,1)))

testClosestPair =
   ((0,0),(3,8)) == closestPair [(0,0), (3,8)] &&
   tClosest      == closestPair tPoints

testLineSide =
   (-1) == lineSide ((1,4),(7,2)) (12,12) &&
     1  == lineSide ((1,4),(7,2)) (0,0) &&
     0  == lineSide ((1,4),(7,2)) (13,0)

testLineSideBool =
   True == isLineSide ((1,4),(7,2)) (0,0) &&
   False == isLineSide ((1,4),(7,2)) (12,12)

testIsHullEdge =
   isHullEdge [] ((2,2),(0,0)) &&
   isHullEdge [(-8,22)] ((2,2),(0,0)) &&
   isHullEdge tPoints ((3,8),(7,7)) &&
   not (isHullEdge tPoints ((3,8),(8,2)))

testConvexHullEdges =
   [] == convexHullEdges [] &&
   [] == convexHullEdges [(3,4)] &&
   7 == length (convexHullEdges tPoints) &&
   all (`elem` tHullEdges) (convexHullEdges tPoints)

testConvexHullPoints =
   [] == convexHullPoints [] &&
   [] == convexHullPoints [(3,4)] &&
   7 == length (convexHullPoints tPoints) &&
   all (`elem` tHullPoints) (convexHullPoints tPoints) &&
   all (`elem` (convexHullPoints tPoints)) tHullPoints

test =
   testDistance &&
   testSqDistance &&
   testPairsFromPoints &&
   testCloserPair &&
   testClosestPair &&
   testLineSide &&
   testIsHullEdge &&
   testConvexHullEdges &&
   testConvexHullPoints
