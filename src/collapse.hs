module Collapse (collapse) where

import AdjacencyMap (AdjacencyMap, KeyFunc, makeAdjacencyMap)
import qualified AdjacencyMap as AM ( findOrDefault, indexOnIncoming
                                    , indexOnOutgoing, insertEdgeWithKey )
import Control.Monad (forM_)
import Control.Monad.State (State(..), runState, state)
import qualified Data.Map as M ( Map, delete, empty, findWithDefault
                               , foldrWithKey, insert, insertWith, keys
                               , keysSet, lookup, member, size, toList )

import Edge as E (Edge(..), makeEdge)
import Map (Map(..), deleteEdge, insertEdge, lookupEdge)
import Point (Point(..))
import Pointset (Coordinate, Pointset, (!?), asciiMapToPoints)
import Transition as Trans (Transition(..), (<.>), direction, directions)


--
-- Map-collapsing utility functions.
--

data PointPartition = PointPartition { looping  :: [Point]
                                     , incoming :: [Point]
                                     , outgoing :: [Point]
                                     }

doUpdateLooping :: Point -> State PointPartition ()
doUpdateLooping p = state $
  \(PointPartition l i o)  -> ((), PointPartition (p:l) i o)

doUpdateOutgoing :: Point -> State PointPartition ()
doUpdateOutgoing p = state $
  \(PointPartition l i o)  -> ((), PointPartition l i (p:o))

doUpdateIncoming :: Point -> State PointPartition ()
doUpdateIncoming p = state $
  \(PointPartition l i o)  -> ((), PointPartition l (p:i) o)

-- Partition all points adjacent to `point` into three groups: "looping",
-- "outgoing", and "incoming". For an edge `(point, point')`, `point'` is said
-- to be "looping" if `point'` has an edge pointing back to `point`. If it does
-- not, it is an "outgoing" edge. Additionally, an "incoming" edge
-- `(point', point)`, where `point` does not have another edge that leads
-- directly to `point'`.
partitionPoints :: Map -> Point -> PointPartition
partitionPoints (Map _ outEdges inEdges) point = partition
  where edgesFrom = M.delete (point, point) $ AM.findOrDefault point outEdges
        edgesTo = M.delete (point, point) $ AM.findOrDefault point inEdges
        runPartitionEdges = runState $
          do
            forM_ (M.keys edgesFrom) $ \(_, p') ->
              if M.member (p', point) edgesTo
                then doUpdateLooping p'
                else doUpdateOutgoing p'
            forM_ (M.keys edgesTo) $ \(p', _) ->
              if M.member (point, p') edgesFrom
                then doUpdateIncoming p'
                else state $ \partition -> ((), partition)
        (_, partition) = runPartitionEdges $ PointPartition [] [] []

sources :: PointPartition -> [Point]
sources partition = (incoming partition) ++ (looping partition)

sinks :: PointPartition -> [Point]
sinks partition = (outgoing partition) ++ (looping partition)

connected :: PointPartition -> [(Point, Point)]
connected partition = loopingToOutgoing ++ incomingToOutgoing
  where loopingToOutgoing = allPairs (looping partition) (outgoing partition)
        incomingToOutgoing = allPairs (incoming partition) (outgoing partition)
        allPairs xs ys = do
          x <- xs
          y <- ys
          [(x, y)]

makeLoopTransition :: Transition -> Point -> Point -> Map -> Transition
makeLoopTransition transSelf point point' mp =
  transFrom <.> transSelf <.> transTo
  where transFrom = lookupEdge point' point mp
        transTo = lookupEdge point point' mp

makeTransitionAroundOrigin :: Point -> Transition -> Point -> Point -> Map -> Transition
makeTransitionAroundOrigin origin transSelf pointA pointB mp  =
  fromPointA <.> transSelf <.> fromOrigin
    where fromPointA = lookupEdge pointA origin mp
          fromOrigin = lookupEdge origin pointB mp

doAddEdge :: Edge -> State Map ()
doAddEdge edge = state $ \mp -> ((), insertEdge edge mp)

doDeleteEdge :: Point -> Point -> State Map ()
doDeleteEdge p p' = state $ \mp -> ((), deleteEdge p p' mp)

getMap :: State Map Map
getMap = state $ \mp -> (mp, mp)

getTranstition :: Point -> Point -> Map -> Transition
getTranstition p p' mp =
  if p == p'
    then Star transition
    else transition
  where edgeset = AM.findOrDefault p (outgoingEdges mp)
        transition = M.findWithDefault Epsilon (p, p') edgeset

doMapUpdate :: (Point -> Point -> Map -> Transition) -> (Point -> Point -> Transition -> Edge) -> (Point, Point) -> State Map ()
doMapUpdate makeTransition makeEdge (pointA, pointB) = do
  updatedMap <- getMap
  let transition = makeTransition pointA pointB updatedMap
  let edge = makeEdge pointA pointB transition
  doAddEdge edge

collapsePoint :: Map -> Point -> Map
collapsePoint mp point = snd $ runAddEdges mp
  where transSelf = getTranstition point point mp
        partition = partitionPoints mp point
        runAddEdges = runState $
          do
            -- TODO(hausdorff): This is entire thing, while more terse than the
            -- previous versions, is kind of awkward and ungainly. Let's
            -- refactor it.
            forM_ [(p, p) | p <- looping partition] $
              doMapUpdate
                (\_ pointB -> makeLoopTransition transSelf point pointB)
                (\pointA pointB transition -> Edge pointB pointB transition)
            forM_ (connected partition) $
              doMapUpdate
                (makeTransitionAroundOrigin point transSelf)
                Edge
            forM_ (sources partition) $ doDeleteEdge point
            forM_ (sinks partition) $
              \outgoingPoint -> doDeleteEdge outgoingPoint point
            doDeleteEdge point point

collapse :: Map -> Coordinate -> Maybe Map
collapse mp (x, y) =
  do
    point <- (points mp) !? (x, y)
    return $ collapsePoint mp point
