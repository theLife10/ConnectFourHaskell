module Board(mkBoard, mkPlayer, mkOpponent, dropInSlot) where

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

dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
dropInSlot bd i p = makeNewColumn bd i newColumn
    where currentColumn = getColumn bd i;
          e = getEmptySpot currentColumn;
          newColumn = insertChip e p currentColumn

getColumn :: [[Int]] -> Int -> [Int]
getColumn bd i = bd !! i

insertChip i chip (x:xs)
    | i == 0 = chip:xs
    | otherwise = x:insertChip (i-1) chip xs

getEmptySpot column = last ( findIndices(==0) column)

makeNewColumn :: [[Int]] -> Int -> [Int] -> [[Int]]
makeNewColumn bd i newColumn = front ++ [newColumn] ++ end
    where front = take (i) bd;
          end = drop(i+1) bd

isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen bd i = elem 0 column
    where column = bd !! i

numSlot:: [[Int]] -> Int
numSlot bd = length bd

isFull:: [[Int]] -> Bool
isFull bd = not ( elem 0 (concat bd))