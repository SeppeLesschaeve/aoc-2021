import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getPolymerRules :: [String] -> [(Char,Char,Char)]
getPolymerRules [] = []
getPolymerRules (rule:rules) = [(firstPoly, secondPoly, inserting)] ++ (getPolymerRules rules)
   where 
      allPolymers = (splitOn " -> " rule)
      leftPattern = allPolymers!!0
      firstPoly   = leftPattern!!0
      secondPoly  = leftPattern!!1
      inserting   = head (allPolymers!!1)
            
getPolymerInput :: String -> (String, [(Char,Char,Char)])
getPolymerInput s = ((head polyLines), (getPolymerRules (tail (tail (polyLines)))))
  where polyLines = lines s
  
insertPolyChar :: [(Char,Char,Char)] -> (Char,Char) -> String
insertPolyChar [] (c1,c2) = [c1]
insertPolyChar ((firstPoly, secondPoly, inserting):rules) (c1,c2)
  | (firstPoly == c1) && (secondPoly == c2) = [c1,inserting]
  | otherwise = insertPolyChar rules (c1,c2)
  
insertPoly :: String -> [(Char,Char,Char)] -> String
insertPoly (c:[]) _ = (c:[])
insertPoly (c1:c2:s) polyRules = (insertPolyChar polyRules (c1,c2)) ++ (insertPoly (c2:s) polyRules)

mostCommon :: [Char] -> Char
mostCommon = head . head . sortBy (flip $ comparing length) . group . sort
  
lstCommon :: [Char] -> Char
lstCommon  = head . head . reverse . sortBy (flip $ comparing length) . group . sort 

countOccurrences :: String -> Char -> Int
countOccurrences [] _ = 0
countOccurrences (c1:s) c2
  | c1 == c2 = 1 + countOccurrences s c2
  | otherwise = countOccurrences s c2
  
countDifference :: String -> Int
countDifference s = most - lst
  where
    most = countOccurrences s (mostCommon s)
    lst = countOccurrences s (lstCommon s)
       

insertPoly10 :: Int -> (String, [(Char,Char,Char)]) -> Int
insertPoly10 0 (s,rules) = countDifference s
insertPoly10 n (s,rules) = insertPoly10 (n-1) ((insertPoly s rules), rules)

main :: IO Int
main = do
  myFile <- openFile "input14.txt" ReadMode
  content <- hGetContents myFile
  return (insertPoly10 10 (getPolymerInput content))     
