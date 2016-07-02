module Map(
      Map(..)
    , makeMap
) where

import Data.List as L
import Data.Vector as V

import MapSquare
import Point

data Map = Map { start         :: Point
               , end           :: Point
            --    , points        :: Grid
            --    , outgoingEdges :: AdjacencyMap
            --    , incomingEdges :: AdjacencyMap
               }

asciiMapToPoints :: [String] -> Vector (Vector Point)
asciiMapToPoints asciiMap = V.fromList points
  where points = do
          (y, row) <- L.zip [0..] asciiMap
          let pointsInRow = do
                (x, asciiSquare) <- L.zip [0..] row
                [Point x y $ MapSquare.fromChar asciiSquare]
          [V.fromList pointsInRow]

allCoordinates :: Vector (Vector Point) -> [(Int, Int)]
allCoordinates map = coordinates
  where rows = V.length map - 1
        rowLength y = L.length (map ! y) - 1
        coordinates = [(x, y) | y <- [0..rows], x <- [0..(rowLength y)]]

makeMap :: [String] -> [(Int, Int)]
makeMap asciiMap = allCoordinates map
    where map = asciiMapToPoints asciiMap
