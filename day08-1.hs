import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getCodedDigit :: [String] -> [String]
getCodedDigit [] = []
getCodedDigit (digit:digits) = codedDigit ++ (getCodedDigit digits)
   where 
      allDigits = (splitOn " | " digit)
      codedDigit = words (allDigits!!1)

getCodedDigits :: String -> [String]
getCodedDigits s = getCodedDigit (lines s)

contains1 :: String -> Bool
contains1 s = (length s) == 2

contains4 :: String -> Bool
contains4 s = (length s) == 4

contains7 :: String -> Bool
contains7 s = (length s) == 3

contains8 :: String -> Bool
contains8 s = (length s) == 7 

countAmountOf1478 :: [String] -> Int -> Int
countAmountOf1478 [] amount = amount
countAmountOf1478 (digit:digits) amount
  | contains1 digit = countAmountOf1478 digits (amount + 1)
  | contains4 digit = countAmountOf1478 digits (amount + 1)
  | contains7 digit = countAmountOf1478 digits (amount + 1)
  | contains8 digit = countAmountOf1478 digits (amount + 1)
  | otherwise = countAmountOf1478 digits amount
  
main :: IO Int
main = do
  myFile <- openFile "input08.txt" ReadMode
  content <- hGetContents myFile
  return (countAmountOf1478 (getCodedDigits content) 0)   
