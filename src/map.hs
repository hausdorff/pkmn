module Map (
      DotGraph(..)
    , Map(..)
    , deleteEdge
    , insertEdge
    , lookupEdge
    , makeMap
) where

import Control.Monad (forM_)
import Control.Monad.State (State(..), runState, state)
import Data.Maybe (mapMaybe)
import qualified Data.List as L ( (++), concatMap, intercalate, length)
import qualified Data.Map as M ( Map, delete, empty, findWithDefault
                               , foldrWithKey, insert, insertWith, keys
                               , keysSet, lookup, member, size, toList )
import qualified Data.Set as S (Set(..), fromList, map, toList, union)
import qualified Data.Vector as V (Vector, (!), length)

import AdjacencyMap (AdjacencyMap, KeyFunc, makeAdjacencyMap)
import qualified AdjacencyMap as AM ( findOrDefault, indexOnIncoming
                                    , indexOnOutgoing, insertEdgeWithKey )
import Edge as E (Edge(..), makeEdge)
import Point (Point(..))
import Pointset (Coordinate, Pointset, (!?), asciiMapToPoints)
import Transition as Trans (Transition(..), (<.>), direction, directions)


-- Gets edges that emanate from the `Point` located at (x, y) in a `Pointset`.
edgesFrom :: Pointset -> Coordinate -> [Edge]
edgesFrom points (x, y) = transitions
  where getOutEdge t = do
          let (x', y') = Trans.direction t x y
          point <- points !? (x, y)
          point' <- points !? (x', y')
          makeEdge point point' t
        transitions = mapMaybe getOutEdge Trans.directions

-- Obtains all edges from a pointset.
allEdges :: Pointset -> [Edge]
allEdges points = concatMap (edgesFrom points) coordinates
  where rows = V.length points - 1
        rowLength y = V.length (points V.! y) - 1
        coordinates = [(x, y) | y <- [0..rows], x <- [0..(rowLength y)]]


--
-- Map utility functions.
--

data Map = Map { points        :: Pointset
               , outgoingEdges :: AdjacencyMap
               , incomingEdges :: AdjacencyMap
               }

instance Show Map where
    show m = L.intercalate "\n" [ "outgoingEdges:", outEdgesString
                              , "incomingEdges", inEdges ]
      where outEdgesString =
              L.intercalate "\n" $
                M.foldrWithKey
                  (\k v acc ->
                    ("\t" ++ show k ++ ": " ++ show v) : acc)
                  []
                  (outgoingEdges m)
            inEdges =
              L.intercalate "\n" $
                M.foldrWithKey
                  (\k v acc ->
                    ("\t" ++ show k ++ ": " ++ show v) : acc)
                  []
                  (incomingEdges m)

class DotGraph d where
  toDot :: d -> String

instance DotGraph Map where
  toDot m = concat [prologue, pointLines, edgeLines, epilogue]
    where prologue = "digraph mp {\n"
          epilogue = "}\n"
          pointLiteral :: Point -> String
          pointLiteral (Point x y _) = concat ["\"(", show x, ",", show y, ")\""]
          pointToDotEntry :: Point -> String
          pointToDotEntry p@(Point x y _) = concat
            [ "\t"
            , pointLiteral p
            , " [shape=circle label=\"#\\n("
            , show x
            , ","
            , show y
            , ")\"   pos=\""
            , show (2*x)
            , ","
            , show (2*y)
            , "!\" style=\"filled\"   fillcolor=palegreen];"
            , "\n"
            ]
          edgeToDotEntry :: Edge -> String
          edgeToDotEntry (Edge p p' t) = concat
            [ "\t"
            , pointLiteral p
            , " -> "
            , pointLiteral p'
            , "[label=\""
            , show t
            , "\" fontcolor=blue];"
            , "\n"
            ]
          -- GROSS GROSS GROSS.
          outgoingPts = M.keysSet $ outgoingEdges m
          incomingPts = M.keysSet $ incomingEdges m
          allPts = S.union outgoingPts incomingPts
          pointLines = concatMap pointToDotEntry allPts
          edges edges = S.fromList $ map (\((p, p'), t) -> Edge p p' t) $ concatMap (M.toList . snd) $ M.toList edges
          inEdges = edges $ incomingEdges m
          outEdges = edges $ outgoingEdges m
          edgeLines = concatMap edgeToDotEntry $ S.toList (S.union outEdges inEdges)

-- Transform an ASCII map into a `Map`.
makeMap :: [String] -> Map
makeMap asciiMap = Map { points = points
                       , outgoingEdges = outgoingEdges
                       , incomingEdges = incomingEdges
                       }
  where points = asciiMapToPoints asciiMap
        edges = allEdges points
        outgoingEdges = makeAdjacencyMap AM.indexOnOutgoing edges
        incomingEdges = makeAdjacencyMap AM.indexOnIncoming edges

lookupEdge :: Point -> Point -> Map -> Transition
lookupEdge p p' mp = case M.lookup p $ outgoingEdges mp of
  Nothing -> Epsilon
  Just edges -> M.findWithDefault Epsilon (p, p') edges

insertEdge :: Edge -> Map -> Map
insertEdge edge mp =
  Map { points = points mp
      , outgoingEdges = AM.insertEdgeWithKey AM.indexOnOutgoing outgoing edge
      , incomingEdges = AM.insertEdgeWithKey AM.indexOnIncoming incoming edge
      }
  where outgoing = outgoingEdges mp
        incoming = incomingEdges mp

deleteEdge :: Point -> Point -> Map -> Map
deleteEdge p p' mp =
  Map { points = points mp
      , outgoingEdges = delete p $ outgoingEdges mp
      , incomingEdges = delete p' $ incomingEdges mp
      }
  where maybeDeleted key adjacencyMap =
          do
            edges <- M.lookup key adjacencyMap
            return $ M.delete (p, p') edges
        delete key adjacencyMap =
          case maybeDeleted key adjacencyMap of
            Nothing -> adjacencyMap
            Just deleted ->
              -- If we've successfully deleted the edge, and that was the last
              -- element in the `AdjacencyMap`, no need to keep an empty
              -- `EdgeSet` around for that point -- go ahead and delete it.
              if M.size deleted == 0
                then M.delete key adjacencyMap
                else M.insert key deleted adjacencyMap
