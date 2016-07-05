
import Data.Foldable
import Data.Map as M
import Data.Vector as V

import Map

m1 = ["#####",
      "#  E#",
      "#E  #",
      "#####"]

m2 = ["####",
      "#SE#",
      "####"]

main = do
    putStrLn "FIRST"
    let mp = Map.makeMap m2
    print mp
    putStrLn "SECOND"
    Data.Foldable.forM_ (collapse mp (1, 1)) print
