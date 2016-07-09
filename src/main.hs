
import Data.Foldable
import Data.Map as M
import Data.Vector as V

import Map (DotGraph(..), Map(..), makeMap)
import Collapse (collapse)

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
    case collapse mp (1, 1) of
      Nothing -> return ()
      Just mp' -> do
        print mp'
        putStrLn $ toDot mp'
