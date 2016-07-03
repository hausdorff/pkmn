module Map(
      Map(..)
    , makeMap
) where

import Prelude hiding ((<*>), Left, Right)

import Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Vector as V

import Edge as E
import MapSquare
import Point
import Transition as Trans


--
-- Pointset utility functions.
--

type Pointset = Vector (Vector Point)

-- Transforms ASCII array into a `Pointset`.
asciiMapToPoints :: [String] -> Pointset
asciiMapToPoints asciiMap = V.fromList points
  where points = do
          (y, row) <- L.zip [0..] asciiMap
          let pointsInRow = do
                (x, asciiSquare) <- L.zip [0..] row
                [Point x y $ MapSquare.fromChar asciiSquare]
          [V.fromList pointsInRow]

-- Enumerates all possible coordinates for a `Pointset`, e.g.: (0,0), (1,0),
-- and so on.
allCoordinates :: Pointset -> [(Int, Int)]
allCoordinates map = coordinates
  where rows = V.length map - 1
        rowLength y = L.length (map ! y) - 1
        coordinates = [(x, y) | y <- [0..rows], x <- [0..(rowLength y)]]

-- Attempts to get a point from a pointset.
getPoint :: Pointset -> Int -> Int -> Maybe Point
getPoint map x y = do
          row <- map !? y
          point <- row !? x
          Just point

-- Gets edges that emanate from the `Point` located at (x, y) in a `Pointset`.
edgesFrom :: Pointset -> Int -> Int -> [Edge]
edgesFrom map x y = transitions
  where getOutEdge t = do
          let (x', y') = Trans.direction t x y
          point <- getPoint map x y
          point' <- getPoint map x' y'
          makeEdge point point' t
        transitions = mapMaybe getOutEdge Trans.directions

-- Obtains all edges from a pointset.
allEdges :: Pointset -> [Edge]
allEdges points = L.concatMap (uncurry $ edgesFrom points) coordinates
  where coordinates = allCoordinates points

filterByType :: Pointset -> MapSquare -> Vector Point
filterByType points squareType = do
          row <- points
          V.filter (\(Point _ _ s) -> s == squareType) row


--
-- Edgeset utility functions.
--

type EdgeSet = M.Map (Point, Point) Trans.Transition

-- Makes an edgeset.
makeEdgeSet :: [Edge] -> EdgeSet
makeEdgeSet edges = edgeset
  where union mp edge@(Edge p p' t) = M.insertWith (<|>) (p, p') t mp
        edgeset = L.foldl union M.empty edges

-- Adds an edge to an edgeset.
addEdge :: EdgeSet -> Edge -> EdgeSet
addEdge mp edge@(Edge p p' t) = M.insertWith (<|>) key val mp
  where key = (p, p')
        val = t


--
-- AdjacencyMap utility functions.
--

type AdjacencyMap = M.Map Point EdgeSet
type KeyFunc = Edge -> Point

indexOnOutgoing :: KeyFunc
indexOnOutgoing (Edge p _ _) = p

indexOnIncoming :: KeyFunc
indexOnIncoming (Edge _ p' _) = p'

-- Inserts an `Edge` into a `AdjacencyMap` using a `KeyFunc` to make a key
-- from the `Edge`.
insertEdgeWithKey :: KeyFunc -> AdjacencyMap -> Edge -> AdjacencyMap
insertEdgeWithKey keyFunc adjacencyMap edge = mp'
  where combineEdgesets _ oldEdgeset = addEdge oldEdgeset edge
        key = keyFunc edge
        defaultValue = makeEdgeSet [edge]
        mp' = M.insertWith combineEdgesets key defaultValue adjacencyMap

-- Make a `AdjacencyMap` using `KeyFunc` to make keys from `Edge`s.
makeAdjacencyMap :: KeyFunc -> [Edge] -> AdjacencyMap
makeAdjacencyMap keyFunc edges = adjacencyMap
  where adjacencyMap = L.foldl (insertEdgeWithKey keyFunc) M.empty edges


--
-- AdjacencyMap utility functions.
--

data Map = Map { start         :: Vector Point
               , end           :: Vector Point
               , points        :: Pointset
               , outgoingEdges :: AdjacencyMap
               , incomingEdges :: AdjacencyMap
               }

instance Show Map where
    show m = show $ start m

-- Transform an ASCII map into a `Map`.
makeMap :: [String] -> Map
makeMap asciiMap = Map { start = filterByType points Entrance
                       , end = filterByType points Exit
                       , points = points
                       , outgoingEdges = outgoingEdges
                       , incomingEdges = incomingEdges
                       }
  where points = asciiMapToPoints asciiMap
        edges = allEdges points
        outgoingEdges = makeAdjacencyMap indexOnOutgoing edges
        incomingEdges = makeAdjacencyMap indexOnIncoming edges
