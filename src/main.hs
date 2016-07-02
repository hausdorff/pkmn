
import Data.Map as M

import Map

m = ["#####",
     "#  E#",
     "#S  #",
     "#####"]

main = mapM print $ M.toList $ Map.makeMap m
