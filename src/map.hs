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

type Pointset = Vector (Vector Point)
type KeyFunc = Edge -> (Point, Point)
type EdgeSet = M.Map (Point, Point) Trans.Transition

data Map = Map { start         :: Point
               , end           :: Point
               , points        :: Pointset
               , outgoingEdges :: EdgeSet
               , incomingEdges :: EdgeSet
               }

asciiMapToPoints :: [String] -> Pointset
asciiMapToPoints asciiMap = V.fromList points
  where points = do
          (y, row) <- L.zip [0..] asciiMap
          let pointsInRow = do
                (x, asciiSquare) <- L.zip [0..] row
                [Point x y $ MapSquare.fromChar asciiSquare]
          [V.fromList pointsInRow]

allCoordinates :: Pointset -> [(Int, Int)]
allCoordinates map = coordinates
  where rows = V.length map - 1
        rowLength y = L.length (map ! y) - 1
        coordinates = [(x, y) | y <- [0..rows], x <- [0..(rowLength y)]]

getPoint :: Pointset -> Int -> Int -> Maybe Point
getPoint map x y = do
          row <- map !? y
          point <- row !? x
          Just point

edgesFrom :: Pointset -> Int -> Int -> [Edge]
edgesFrom map x y = transitions
  where getOutEdge t = do
          let (x', y') = Trans.direction t x y
          point <- getPoint map x y
          point' <- getPoint map x' y'
          makeEdge point point' t
        transitions = mapMaybe getOutEdge Trans.directions

outgoingEdge :: KeyFunc
outgoingEdge (Edge p p' _) = (p, p')

incomingEdge :: KeyFunc
incomingEdge (Edge p p' _) = (p', p)

aggregateEdgeSet :: [Edge] -> KeyFunc -> EdgeSet
aggregateEdgeSet edges keyFunc = adjacencyMap
  where union mp edge@(Edge _ _ t) = M.insertWith (<|>) (keyFunc edge) t mp
        adjacencyMap = L.foldl union M.empty edges

makeMap :: [String] -> EdgeSet
makeMap asciiMap = incomingEdges
  where mp = asciiMapToPoints asciiMap
        coordinates = allCoordinates mp
        allEdges = L.concatMap (uncurry $ edgesFrom mp) coordinates
        outgoingEdges = aggregateEdgeSet allEdges outgoingEdge
        incomingEdges = aggregateEdgeSet allEdges incomingEdge
