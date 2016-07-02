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
type EdgeSet = M.Map (Point, Point) Trans.Transition
type KeyFunc = Edge -> Point
type TransitionMap = M.Map Point EdgeSet

data Map = Map { start         :: Point
               , end           :: Point
               , points        :: Pointset
               , outgoingEdges :: TransitionMap
               , incomingEdges :: TransitionMap
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

makeEdgeSet :: [Edge] -> EdgeSet
makeEdgeSet edges = edgeset
  where union mp edge@(Edge p p' t) = M.insertWith (<|>) (p, p') t mp
        edgeset = L.foldl union M.empty edges

addEdge :: EdgeSet -> Edge -> EdgeSet
addEdge mp edge@(Edge p p' t) = M.insertWith (<|>) key val mp
  where key = (p, p')
        val = t

indexOnOutgoing :: KeyFunc
indexOnOutgoing (Edge p _ _) = p

indexOnIncoming :: KeyFunc
indexOnIncoming (Edge _ p' _) = p'

unionTransitions :: KeyFunc -> TransitionMap -> Edge -> TransitionMap
unionTransitions keyFunc mp edge = mp'
  where combineEdgesets _ oldEdgeset = addEdge oldEdgeset edge
        key = keyFunc edge
        defaultValue = makeEdgeSet [edge]
        mp' = M.insertWith combineEdgesets key defaultValue mp

makeTransitionMap :: KeyFunc -> [Edge] -> TransitionMap
makeTransitionMap keyFunc edges = transitionMap
  where transitionMap = L.foldl (unionTransitions keyFunc) M.empty edges

makeMap :: [String] -> TransitionMap
makeMap asciiMap = incomingEdges
  where mp = asciiMapToPoints asciiMap
        coordinates = allCoordinates mp
        allEdges = L.concatMap (uncurry $ edgesFrom mp) coordinates
        outgoingEdges = makeTransitionMap indexOnOutgoing allEdges
        incomingEdges = makeTransitionMap indexOnIncoming allEdges
