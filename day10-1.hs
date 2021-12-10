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

getCorruption :: [Int] -> [Int] -> Int
getCorruption [] _ = 0
getCorruption (bracket:brackets) bracketStack
  | bracket > 4 = getCorruption brackets (bracket:bracketStack)
  | (bracket == 1) && ((head bracketStack) /= 5) = bracket
  | (bracket == 2) && ((head bracketStack) /= 6) = bracket
  | (bracket == 3) && ((head bracketStack) /= 7) = bracket
  | (bracket == 4) && ((head bracketStack) /= 8) = bracket
  | otherwise = getCorruption brackets (tail bracketStack) 

filterCorrupted :: [[Int]] -> [Int]
filterCorrupted [] = []
filterCorrupted (syntaxLine:syntaxLines)
  | corruption == 0 = remainingFilter
  | otherwise       = corruption:remainingFilter
  where 
    corruption = getCorruption syntaxLine []
    remainingFilter = filterCorrupted syntaxLines
    
getCorruptionScore :: [Int] -> Int
getCorruptionScore [] = 0
getCorruptionScore (corruptionScore:corruptionScores)
  | corruptionScore == 1 = 3 + (getCorruptionScore corruptionScores)
  | corruptionScore == 2 = 57 + (getCorruptionScore corruptionScores)
  | corruptionScore == 3 = 1197 + (getCorruptionScore corruptionScores)
  | corruptionScore == 4 = 25137 + (getCorruptionScore corruptionScores)
  
main :: IO Int
main = do
  myFile <- openFile "input10.txt" ReadMode
  content <- hGetContents myFile
  return (getCorruptionScore (filterCorrupted (getSyntaxLines content)))
