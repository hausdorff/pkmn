module AdjacencyMap (
      AdjacencyMap
    , KeyFunc
    , findOrDefault
    , indexOnIncoming
    , indexOnOutgoing
    , insertEdgeWithKey
    , makeAdjacencyMap
) where

import qualified Data.Map as M (Map, empty, findWithDefault, insertWith)

import Edge (Edge(..))
import Edgeset (EdgeSet, addEdge, makeEdgeSet)
import Point (Point)


--
-- AdjacencyMap utility functions.
--

type AdjacencyMap = M.Map Point EdgeSet
type KeyFunc = Edge -> Point

indexOnOutgoing :: KeyFunc
indexOnOutgoing (Edge p _ _) = p

indexOnIncoming :: KeyFunc
indexOnIncoming (Edge _ p' _) = p'

findOrDefault :: Point -> AdjacencyMap -> EdgeSet
findOrDefault = M.findWithDefault M.empty

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
  where adjacencyMap = foldl (insertEdgeWithKey keyFunc) M.empty edges
