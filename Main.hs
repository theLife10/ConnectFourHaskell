import System.IO
import Data.List
import Board

main = do
  let bd = mkboard 7 6
  let p = mkPlayer
  playConnectFour bd p

playConnectFour :: [[Int]] -> Int -> IO()



getX = do
     putStrLn "Enter a positive value?"
     line <- getLine
     let parsed = reads line :: [(Integer, String)] in
       if length parsed == 0
       then getX'
       else let (x, _) = head parsed in
         if x > 0
         then return x
         else getX'
     where
       getX' = do
         putStrLn "Invalid input!"
         getX
