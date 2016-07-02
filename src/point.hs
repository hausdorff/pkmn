module Point (
      Point(..)
    , toChar
    , getMapSquare
    , positionEqual
    ) where

import MapSquare hiding (toChar)

data Point = Point Int Int MapSquare
    deriving (Eq, Ord)

instance Show Point where
    show (Point p p' sqr) = show (p, p', sqr)

toChar :: Point -> Char
toChar p = case show p of
    [c] -> c

positionEqual :: Point -> Point -> Bool
positionEqual (Point x1 y1 _) (Point x2 y2 _) = x1 == x2 && y1 == y2

getMapSquare :: Point -> MapSquare
getMapSquare (Point _ _ square) = square
