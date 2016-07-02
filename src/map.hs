module Map(
      Map(..)
    , makeMap
) where

import Prelude hiding ((<*>), Left, Right)

import Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Vector as V

import MapSquare
import Point
import qualified Transition as Trans

type Pointset = Vector (Vector Point)

data Map = Map { start         :: Point
               , end           :: Point
            --    , points        :: Grid
            --    , outgoingEdges :: AdjacencyMap
            --    , incomingEdges :: AdjacencyMap
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

transitionsFrom :: Pointset -> Int -> Int -> [(Trans.Transition, (Int, Int))]
transitionsFrom map x y = transitions
  where validTransition t = do
          let (x', y') = Trans.direction t x y
          row <- map !? y'
          point <- row !? x'
          Just (t, (x', y'))
        transitions = mapMaybe validTransition Trans.directions

makeMap :: [String] -> [(Trans.Transition, (Int, Int))]
makeMap asciiMap = transitions
  where map = asciiMapToPoints asciiMap
        coordinates = allCoordinates map
        transitions = L.concatMap (uncurry $ transitionsFrom map) coordinates
