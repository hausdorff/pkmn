module Point (
      Point(..)
    , toChar
    ) where

import MapSquare hiding (toChar)

data Point = Point Int Int MapSquare
    deriving (Eq, Ord)

instance Show Point where
    show (Point p p' sqr) = show (p, p', sqr)

toChar :: Point -> Char
toChar p = case show p of
    [c] -> c
