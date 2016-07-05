module Pointset (
      Coordinate
    , Pointset
    , (!?)
    , asciiMapToPoints
) where

import qualified Data.Vector as V (Vector, (!?), fromList)

import MapSquare
import Point


--
-- Pointset utility functions.
--

type Coordinate = (Int, Int)
type Pointset = V.Vector (V.Vector Point)

-- Transforms ASCII array into a `Pointset`.
asciiMapToPoints :: [String] -> Pointset
asciiMapToPoints asciiMap = V.fromList points
  where points = do
          (y, row) <- zip [0..] asciiMap
          let pointsInRow = do
                (x, asciiSquare) <- zip [0..] row
                [Point x y $ MapSquare.fromChar asciiSquare]
          [V.fromList pointsInRow]

(!?) :: Pointset -> Coordinate -> Maybe Point
(!?) points (x, y) = do
          row <- points V.!? y
          point <- row V.!? x
          Just point
