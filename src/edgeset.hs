module Edgeset (
      EdgeSet
    , addEdge
    , makeEdgeSet
) where

import qualified Data.Map as M (Map, empty, insertWith)

import Edge as E (Edge(..), makeEdge)
import Point (Point)
import Pointset (Coordinate, Pointset, (!?), asciiMapToPoints)
import Transition as Trans (Transition(..), (<|>), direction, directions)


--
-- Edgeset utility functions.
--

type EdgeSet = M.Map (Point, Point) Trans.Transition

-- Makes an edgeset.
makeEdgeSet :: [Edge] -> EdgeSet
makeEdgeSet edges = edgeset
  where union mp edge@(Edge p p' t) = M.insertWith (<|>) (p, p') t mp
        edgeset = foldl union M.empty edges

-- Adds an edge to an edgeset.
addEdge :: EdgeSet -> Edge -> EdgeSet
addEdge mp edge@(Edge p p' t) = M.insertWith (<|>) key val mp
  where key = (p, p')
        val = t
