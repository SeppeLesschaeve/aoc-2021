import System.IO  
import Control.Monad
import Data.List
import Data.Ord

digs :: Int -> [Int]
digs 0 = []
digs x = (digs (x `div` 10)) ++ [x `mod` 10]

toBinaryHelper :: [Int] -> Int -> Int
toBinaryHelper [] 0 = 0
toBinaryHelper (x:xs) n 
  | x == 1 = (x * (2 ^ (n-1))) + (toBinaryHelper xs (n-1))
  | x == 0 = toBinaryHelper xs (n-1)

toBinary :: Int -> Int
toBinary i = toBinaryHelper d (length d)   where d = digs i

readRate :: String -> Int
readRate = toBinary . read

mostCommon :: [Char] -> Char
mostCommon = head . head . sortBy (flip $ comparing length) . group . sort
  
lstCommon :: [Char] -> Char
lstCommon  = head . head . reverse . sortBy (flip $ comparing length) . group . sort

getPowerHelper :: [String] -> String -> String -> Int 
getPowerHelper [] gamma epsilon = (readRate gamma) * (readRate epsilon)
getPowerHelper (b:bs) gamma epsilon = getPowerHelper bs (gamma ++ [mostCommon b]) (epsilon ++ [lstCommon b])

getPower :: [String] -> Int
getPower m = getPowerHelper (transpose m) "" ""

main :: IO Int
main = do
  myFile <- openFile "input03.txt" ReadMode
  content <- hGetContents myFile
  return (getPower (lines content))
