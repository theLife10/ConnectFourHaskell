module Board(mkBoard, mkPlayer, mkOpponent) where

import Data.List
import Data.Array


mkBoard :: Int -> Int ->[[Int]]
mkBoard m n = replicate m (replicate n 0)
--player 1
mkPlayer :: Int
mkPlayer = 1
--oponent 
mkOpponent :: Int
mkOpponent = 2