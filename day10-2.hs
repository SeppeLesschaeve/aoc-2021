import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

readBracket :: Char -> Int
readBracket c 
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4
  | c == '(' = 5
  | c == '[' = 6
  | c == '{' = 7
  | c == '<' = 8

getSyntaxLine :: String -> [Int]
getSyntaxLine [] = []
getSyntaxLine (bracket:brackets) = (readBracket bracket):(getSyntaxLine brackets) 

getSyntaxLines :: String -> [[Int]]
getSyntaxLines s = map getSyntaxLine (lines s)

getIncomplete :: [Int] -> [Int] -> (Int,[Int])
getIncomplete [] bracketStack = (0,bracketStack)
getIncomplete (bracket:brackets) bracketStack
  | bracket > 4 = getIncomplete brackets (bracket:bracketStack)
  | (bracket == 1) && ((head bracketStack) /= 5) = (bracket,bracketStack)
  | (bracket == 2) && ((head bracketStack) /= 6) = (bracket,bracketStack)
  | (bracket == 3) && ((head bracketStack) /= 7) = (bracket,bracketStack)
  | (bracket == 4) && ((head bracketStack) /= 8) = (bracket,bracketStack)
  | otherwise = getIncomplete brackets (tail bracketStack) 

filterIncomplete :: [[Int]] -> [[Int]]
filterIncomplete [] = []
filterIncomplete (syntaxLine:syntaxLines)
  | incompletion == 0 = bracketStack:remainingFilter
  | otherwise       =   remainingFilter
  where 
    (incompletion, bracketStack) = getIncomplete syntaxLine []
    remainingFilter = filterIncomplete syntaxLines
    
getIncompleteScore :: [Int] -> Int -> Int 
getIncompleteScore [] score = score
getIncompleteScore (bracket:brackets) score 
  | bracket == 5 = getIncompleteScore brackets (tempScore + 1)
  | bracket == 6 = getIncompleteScore brackets (tempScore + 2)
  | bracket == 7 = getIncompleteScore brackets (tempScore + 3)
  | bracket == 8 = getIncompleteScore brackets (tempScore + 4)
  where tempScore = score * 5    

getIncompleteScores :: [[Int]] -> [Int]
getIncompleteScores [] = []
getIncompleteScores (bracketStack:bracketStacks) = (getIncompleteScore bracketStack 0):(getIncompleteScores bracketStacks)
   
getMiddleIncompleteScore :: String -> Int
getMiddleIncompleteScore s = incompleteScores!!(div (length incompleteScores) 2) 
   where incompleteScores = sort (getIncompleteScores ( filterIncomplete (getSyntaxLines s)))
  
main :: IO Int
main = do
  myFile <- openFile "input10.txt" ReadMode
  content <- hGetContents myFile
  return (getMiddleIncompleteScore content)
