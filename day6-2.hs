import System.IO  
import Control.Monad
import Data.List.Split
import Data.List

getLanternFishes :: String -> [Int]
getLanternFishes s = map read (splitOn "," s)
  
fillStackWithFishesHelper :: [[Int]] -> Int -> Int -> [[Int]]
fillStackWithFishesHelper [] _ _ = []
fillStackWithFishesHelper (row:rows) fish rowIndex 
  | fish == rowIndex = [amount + 1]:rows
  | otherwise        = row:(fillStackWithFishesHelper rows fish (rowIndex + 1))
  where amount = head row

fillStackWithFishes :: [Int] -> [[Int]] -> [[Int]]
fillStackWithFishes [] stack = stack
fillStackWithFishes (fish:fishes) stack = fillStackWithFishes fishes newStack
  where newStack = fillStackWithFishesHelper stack fish 0 
  
iterateDaysHelper :: [[Int]] -> Int -> [[Int]]
iterateDaysHelper (r0:r1:r2:r3:r4:r5:r6:r7:r8:[]) zeroFishes = r1:r2:r3:r4:r5:r6:newR6:r8:newR8:[]   
  where 
    newR6 = [(head r7) + zeroFishes]
    newR8 = [zeroFishes]
    
iterateDays :: [[Int]] -> Int -> Int -> Int
iterateDays stack 0 total = total
iterateDays stack n total = iterateDays newStack (n-1) (total + zeroFishes)
  where 
    zeroFishes = head (head stack)
    newStack = iterateDaysHelper stack zeroFishes

calculate256 :: [Int] -> [[Int]] -> Int -> Int
calculate256 fishes stack total = iterateDays newStack 256 total
  where newStack = fillStackWithFishes fishes stack

main :: IO Int
main = do
  myFile <- openFile "input6.txt" ReadMode
  content <- hGetContents myFile
  return (calculate256 (getLanternFishes content) [[0],[0],[0],[0],[0],[0],[0],[0],[0]] (length (getLanternFishes content))) 
