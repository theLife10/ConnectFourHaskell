
import System.IO
import Data.List

import Board

main = do
  let bd = mkBoard 7 6
  let p = mkPlayer
  putStrLn (boardToStr playerToChar bd 0)
  putStrLn ""
  do play bd p

--Creates connect four game
play :: [[Int]] -> Int -> IO()
play bd p = do
  slot <- readSlot bd p
  if (slot == (-1))
    then putStrLn "Exiting Game"

  else do
    let newBoard = dropInSlot bd slot p
    let stringBoard = boardToStr playerToChar newBoard 0
    putStrLn stringBoard

    if(isWonBy newBoard p)
    then do
      putStr (playerToChar p)
      putStr "won :)"
      putStrLn ""

    else do
        play newBoard (changePlayer p)

--Returns in user slot
readSlot :: [[Int]] -> Int -> IO(Int)
readSlot bd p = do
  let check = False
  putStrLn ("Player " ++ playerToChar p ++ "'s turn")
  putStrLn "Enter slot value 0-6 to place token, or -1 to Exit Game"

  line <- getLine
  let parsed = reads line :: [(Int, String)] in
    if length parsed == 0
    then tryAgain

    else let (x, _) = head parsed in
      if (x >= 0 && x < 7 && (isSlotOpen bd x))
      then return x

      else do
        if(x == (-1))
          then return (-1)

          else do tryAgain
  where
    tryAgain = do
      putStrLn "Invalid input!"
      readSlot bd p

--Used to change next player
changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1

--Turns player into string
playerToChar :: Int -> String
playerToChar val
  | (val == 1) = "O "
  | (val == 2) = "X "
  | otherwise = ". "
