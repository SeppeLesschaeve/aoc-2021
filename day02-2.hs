import System.IO  
import Control.Monad

getContent :: String -> [String]
getContent s = getContentHelper (lines s)

getContentHelper :: [String] -> [String]
getContentHelper [] = []
getContentHelper (content:contents) = (words content) ++ (getContentHelper contents)

readAmount :: String -> Int
readAmount = read

getMultipleHD :: [String] -> Int -> Int -> Int -> Int
getMultipleHD [] hor depth aim =  hor * depth
getMultipleHD (dir:am:xs) hor depth aim
  | dir == "forward" = getMultipleHD xs (hor + (readAmount am)) (depth + (aim * (readAmount am))) aim
  | dir == "down"    = getMultipleHD xs hor depth (aim + (readAmount am))
  | dir == "up"      = getMultipleHD xs hor depth (aim - (readAmount am))


main :: IO Int
main = do
  myFile <- openFile "input02.txt" ReadMode
  content <- hGetContents myFile
  return (getMultipleHD (getContent content) 0 0 0)
