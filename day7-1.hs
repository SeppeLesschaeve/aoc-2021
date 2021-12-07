import System.IO  
import Control.Monad
import Data.List.Split
import Data.List

getPositions :: String -> [Int]
getPositions s = map read (splitOn "," s)

getFuelHelper :: [Int] -> Int -> Int
getFuelHelper [] _ = 0
getFuelHelper (pos:positions) position = (abs(pos-position)) + (getFuelHelper positions position)

getMinimalPos :: [Int] -> [Int] -> Int -> (Int,Int)
getMinimalPos [] positions minimalPos = (minimalPos,getFuelHelper positions minimalPos)
getMinimalPos (pos:positions) listOfPositions minimalPos 
  | newMinimalPos < curMinimalPos = getMinimalPos positions listOfPositions pos
  | otherwise                     = getMinimalPos positions listOfPositions minimalPos
  where 
    newMinimalPos = getFuelHelper listOfPositions pos
    curMinimalPos = getFuelHelper listOfPositions minimalPos

getAllPossiblePositions :: [Int] -> [Int]
getAllPossiblePositions positions = [pos | pos <- [(minimum positions) .. (maximum positions)]]
    
getFuel :: [Int] -> (Int,Int)
getFuel positions = getMinimalPos (getAllPossiblePositions positions) positions 1 

main :: IO (Int,Int)
main = do
  myFile <- openFile "input7.txt" ReadMode
  content <- hGetContents myFile
  return (getFuel (getPositions content))
