module Board(mkBoard, mkPlayer, mkOpponent, dropInSlot, boardToStr, isSlotOpen, isWonBy) where

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
          newColumn = putPlayerToken e p currentColumn

--Takes in board, and column number, returns column
getColumn :: [[Int]] -> Int -> [Int]
getColumn bd i = bd !! i

--Puts player at certain slot i
putPlayerToken i p (x:xs)
    | i == 0 = p:xs
    | otherwise = x:putPlayerToken (i-1) p xs

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

--Prints board
boardToStr :: (Int -> String) -> [[Int]] -> Int -> String
boardToStr playerToChar bd 6 = "" --6 represents last row
boardToStr playerToChar bd i = val bd playerToChar i 0 ++ boardToStr playerToChar bd (i+1)
  where
    val :: [[Int]] -> (Int -> String) -> Int -> Int -> String
    val bd playerToChar y 7 = "\n" -- 7 represents last column
    val bd playerToChar y x = playerToChar (bd !! x !! y) ++ val bd playerToChar y (x+1) -- x gets the column, y is the row

-- Checks for a winner
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p
  | ((rowCheck == True) || (colCheck == True)) = True
  | otherwise = False
  where
    rowCheck = winRowCheck bd p 0
    colCheck = winColCheck bd p 0

-- Checks column for win, takes in board, player, starting row, returns true if player won
winColCheck :: [[Int]] -> Int -> Int -> Bool
winColCheck bd p i
  | (check == True) = True
  | (i == 6) = False
  |otherwise = winColCheck bd p (i+1)
  where check = colCheck (bd !! i) p 0 0


--Takes in the column, player, and row
colCheck :: [Int] -> Int -> Int -> Int -> Bool
colCheck col p i count
  | (count == 3) = True
  | (i == 5) = False
  | ((col !! i) == p) && ((col !! (i+1)) == p) = colCheck col p (i+1) (count+1)
  |otherwise =  colCheck col p (i+1) count

-- Takes in board, player, and row
winRowCheck :: [[Int]] -> Int -> Int -> Bool
winRowCheck bd p i
  |(i == 6) = False
  |(check == True) = True
  |otherwise = winRowCheck bd p (i+1)
  where check = rowCheck bd p i 0 0

--Takes in board, player, y, x, and win Count
rowCheck :: [[Int]] -> Int -> Int -> Int -> Int -> Bool
rowCheck bd p y x count
  | (x == 7) = False
  | (count == 4) = True
  |((bd !! x !! y) == p) && ((bd !! x !! y) == p) = rowCheck bd p y (x+1) (count+1)
  |otherwise = rowCheck bd p y (x+1) count
