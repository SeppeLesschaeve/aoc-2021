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

filterReportHelper :: Char -> Int -> [String] -> [String]
filterReportHelper _ _ [] = []
filterReportHelper c i (x:xs) 
  | (x!!i) == c = x:(filterReportHelper c i xs)
  | otherwise     = filterReportHelper c i xs
  
countOccurrence :: Char -> String -> Int
countOccurrence c [] = 0
countOccurrence c (x:xs) 
 | c == x = 1 + countOccurrence c xs
 | otherwise = countOccurrence c xs 
  
equalCommon :: String -> Bool
equalCommon xs = countOccurrence '0' xs == countOccurrence '1' xs

filterReport :: [String] -> Int -> Char -> ([Char] -> Char) -> Int
filterReport (x:[]) _ _ _ = readRate x
filterReport (x:xs) i c f
   | equalCommon currentRow = filterReport (filterReportHelper c i (x:xs)) (i+1) c f
   | otherwise  = filterReport (filterReportHelper (f currentRow) i (x:xs)) (i+1) c f
   where currentRow = (transpose (x:xs))!!i

getOxygen :: [String] -> Int
getOxygen m = filterReport m 0 '1' mostCommon

getCO2 :: [String] -> Int
getCO2 m = filterReport m 0 '0' lstCommon

getLife :: [String] -> Int
getLife m = (getOxygen m) * (getCO2 m)

main :: IO Int
main = do
  myFile <- openFile "input3.txt" ReadMode
  content <- hGetContents myFile
  return (getLife (lines content))
