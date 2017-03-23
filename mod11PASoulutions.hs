-- Mod11PA.hs -- CS 430 Spring 2017 Module 11 PA.
-- John C. Bowers

------------------------------------------------------------------------------
-- Part 1: Closest pairs of point.
-- A closest pair of a set of points is a pair from the set whose distance is
-- no greater than the distance between any other pair of points from the set.

type Point a = (a,a)

-- Determine the true distance between two points.
distance :: (Real a, Floating b) => Point a -> Point a -> b
distance p q = sqrt $ sqDistance p q

--My mistake here was not using realToFrac to get a Rational number
-- which is a parent of Floating (which is what we wanted)

-- Determine the square of the distance between two points.
-- This is a faster alternative to finding the true distance in the closest pairs algorithm.
sqDistance :: (Real a, Floating b) => Point a -> Point a -> b
sqDistance (x1,y1) (x2,y2) = realToFrac ((dx*dx) + (dy*dy))
  where
    dx = x2 - x1
    dy = y2 - y1

type Pair a = (Point a, Point a)

-- Generate all pairs of points from a list of points.
-- Each point should appear exactly once in the result.
pairsFromPoints :: Real a => [Point a] -> [Pair a]
pairsFromPoints [] = []
pairsFromPoints (p:pts) = (map (pairUp p) pts) ++ (pairsFromPoints pts)
  where
    pairUp :: Point a -> Point a -> Pair a
    pairUp p q = (p,q)

-- Determine which of two pairs of points is the closer.
closerPair :: Real a => Pair a -> Pair a -> Pair a
closerPair (p1,p2) (q1,q2)
  | (sqDistance p1 p2) <= (sqDistance q1 q2) = (p1, p2)
  | otherwise = (q1, q2)

-- Find the closest pair of points given a list of points (brute force).
closestPair :: Real a => [Point a] -> Pair a
closestPair points = foldl1 closerPair $ pairsFromPoints points

---------------------------------------------------------------------------
-- Part 2: Find the convex hull of a set of points.
-- A convex hull of a set of points is the subset of the points forming the
-- smallest convex polygon enclosing all of them.

type Edge a = (Point a, Point a)

-- Given an edge (that determines a line), say whether, as one moves along the
-- edge from its first to its second point, one must turn right (1) or left (-1)
-- to get to the point, or whether the point is on the line itself (0).
lineSide :: Real a => Edge a -> Point a -> a
lineSide ((e1x,e1y),(e2x,e2y)) (px,py) = signum ((px-e1x)*(e2y-e1y) - (e2x-e1x)*(py-e1y))

-- isHullEdge returns true just in case all points are on the same side of or
-- on the line determined by an edge.
isHullEdge :: Real a => [Point a] -> Edge a -> Bool
isHullEdge points edge = allSame ((filter (/=0) (map (lineSide edge) points)))
  where
    allSame [] = True
    allSame (x:[]) = True
    allSame (x:xs) = foldr1 (&&) (map (==x) xs)

-- Given a set of points, produce a list of all the edges in the convex hull for that set.
-- The result list has no duplicates.
convexHullEdges :: Real a => [Point a] -> [Edge a]
convexHullEdges points = filter (isHullEdge points) (pairsFromPoints points)

-- Given a set of points, produce a list of all the points forming the convex
-- hull for that set. The result has no duplicates.
convexHullPoints :: Real a => [Point a] -> [Point a]
convexHullPoints points =
  let
    edges = convexHullEdges points
  in filter (incidentToAnEdge edges) points
  where
    incidentToAnEdge [] point = False
    incidentToAnEdge ((p1, p2):pts) point
      | p1 == point || p2 == point = True
      | otherwise = incidentToAnEdge pts point

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
