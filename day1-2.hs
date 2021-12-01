import System.IO  
import Control.Monad

getDepths :: String -> [Int] 
getDepths s = map read (lines s)

getIncreases :: [Int] -> Int -> Int
getIncreases [] increases = increases
getIncreases (depth1:[]) increases = increases
getIncreases (depth1:depth2:depths) increases
  | depth2 > depth1 = getIncreases (depth2:depths) (increases + 1)
  | otherwise       = getIncreases (depth2:depths) increases
  
getDepthsSum3 :: [Int] -> [Int]
getDepthsSum3 (depth1:depth2:depth3:[]) = [depth1 + depth2 + depth3 ]
getDepthsSum3 (depth1:depth2:depth3:depths) = (depth1 + depth2 + depth3):(getDepthsSum3 (depth2:depth3:depths))

main :: IO Int
main = do
  myFile <- openFile "input1.txt" ReadMode
  content <- hGetContents myFile
  return (getIncreases (getDepthsSum3 (getDepths content)) 0)
  
