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

-- Takes in board, column number, and value, puts the value in the board at the column number
dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
--bd is the board, i is the column, p is the player marker
dropInSlot bd i p = makeNewColumn bd i newColumn
    where currentColumn = getColumn bd i;
          e = getEmptySpot currentColumn; --e is emptyPosition
          newColumn = insertChip e p currentColumn

--Takes in board, and column number, returns column
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

--Takes in board and column number, returns True or False if the column is not full
isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen bd i = elem 0 column
    where column = bd !! i

-- Takes in board and returns the amount of columns
numSlot:: [[Int]] -> Int
numSlot bd = length bd

--Takes in board and returns the amount of rows
numRow:: [[Int]] -> Int
numRow bd = length ( bd !! 0)

--Takes in board, row, and column and returns the tokens
getToken:: [[Int]] -> Int -> Int -> Int
getToken bd r c = (bd !! r) !! c

--Takes in board and returns if the board is full (True if its full, False if not)
isFull:: [[Int]] -> Bool
isFull bd = not ( elem 0 (concat bd))




boardToStr :: (Int -> String) -> [[Int]] -> Int -> String
boardToStr playerToChar bd 7 = ""
boardToStr playerToChar bd i = val bd playerToChar i 0 ++ boardToStr playerToChar bd (i+1)
  where
    val :: [[Int]] -> (Int -> String) -> Int -> Int -> String
    val bd playerToChar y 6 = "\n"
    val bd playerToChar y x = playerToChar (bd !! x !! y) ++ val bd playerToChar y (x+1)





playerToChar :: Int -> String
playerToChar val
 | (val == 1) = "O "
 | (val == 2) = "X "
 | otherwise = "."
