import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord
import Data.Char

isSmallCave :: String -> Bool
isSmallCave = not . any isUpper

getPathHelper :: [String] -> [(String,String)]
getPathHelper [] = []
getPathHelper (path:paths) = (begin,stop):(getPathHelper paths)
   where 
      places = splitOn "-" path
      begin  = places!!0
      stop   = places!!1

getPaths :: String -> [(String,String)]
getPaths s = getPathHelper (lines s)

findAmountOfStarts :: [(String,String)] -> Int
findAmountOfStarts [] = 0
findAmountOfStarts ((begin, stop):caves) 
  | (begin == "start") || (stop == "start") = 1 + (findAmountOfStarts caves) 
  | otherwise        = findAmountOfStarts caves 
  
checkOccurrences :: [String] -> String -> Int
checkOccurrences [] _ = 0
checkOccurrences (seenSmallCave:seenSmallCaves) twice 
  | seenSmallCave == twice = 1 + (checkOccurrences seenSmallCaves twice)  
  | otherwise              = checkOccurrences seenSmallCaves twice
  
extendPathStop :: (String, String) -> ([String],[String],String) -> [([String],[String],String)]
extendPathStop (begin,stop) (path,seenSmallCaves,twice)
  | (not (isSmallCave stop))         = [(stop:path, seenSmallCaves,twice)]
  | (stop == twice) && ((checkOccurrences seenSmallCaves stop)  < 2) = choiceMade
  | (not (twice == "")) && (not (elem stop seenSmallCaves)) = [((stop:path),(stop:seenSmallCaves),twice)]
  | (twice == "") && (not (stop == "start" || stop == "end" || (elem stop seenSmallCaves))) = choiceMade ++ choiceNotMade
  | (twice == "") && (not (elem stop seenSmallCaves)) && (stop == "end") = choiceNotMade
  | otherwise                         = []
  where
    choiceMade = [((stop:path),(stop:seenSmallCaves),stop)]
    choiceNotMade = [((stop:path),(stop:seenSmallCaves),"")]

extendPathBegin :: (String, String) -> ([String],[String],String) -> [([String],[String],String)]
extendPathBegin (begin,stop) (path,seenSmallCaves,twice)
  | (not (isSmallCave begin))         = [(begin:path, seenSmallCaves,twice)]
  | (begin == twice) && ((checkOccurrences seenSmallCaves begin)  < 2) = choiceMade
  | (not (twice == "")) && (not (elem begin seenSmallCaves)) = [((begin:path),(begin:seenSmallCaves),twice)]
  | (twice == "") && (not (begin == "start" || begin == "end" || (elem begin seenSmallCaves))) = choiceMade ++ choiceNotMade
  | (twice == "") && (not (elem begin seenSmallCaves)) && (begin == "end") = choiceNotMade
  | otherwise                         = []
  where
    choiceMade = [((begin:path),(begin:seenSmallCaves),begin)]
    choiceNotMade = [((begin:path),(begin:seenSmallCaves),"")]

extendPath :: [(String, String)] -> ([String],[String],String) -> [([String],[String],String)]
extendPath [] path = []
extendPath ((begin,stop):possibleCaves) (path,seenSmallCaves,twice)
  | (head path) == begin = (extendPathStop (begin,stop) (path,seenSmallCaves,twice)) ++ remainingExtend
  | (head path) == stop  = (extendPathBegin (begin,stop) (path,seenSmallCaves,twice)) ++ remainingExtend
  | otherwise            = remainingExtend
  where remainingExtend = extendPath possibleCaves (path,seenSmallCaves,twice)

extendAllPaths :: [([String],[String],String)] -> [(String, String)] -> [([String],[String],String)]
extendAllPaths [] _ = []
extendAllPaths (path:paths) possibleCaves
  | (head currPath) == "end" = path:(extendAllPaths paths possibleCaves)
  | otherwise                = (extendPath possibleCaves path) ++ (extendAllPaths paths possibleCaves)
  where 
    (currPath, currSeenSmallCaves,twice) = path

areAllEnding :: [([String],[String],String)] -> Bool
areAllEnding [] = True
areAllEnding ((path,seenSmallCaves,twice):paths) 
  | (head path) == "end" = areAllEnding paths
  | otherwise            = False 
    
getAllPathsHelper :: [(String,String)] -> [([String],[String],String)] -> [([String],[String],String)]   
getAllPathsHelper possibleCaves allPaths 
  | areAllEnding allPaths = allPaths
  | otherwise             = getAllPathsHelper possibleCaves extendedAllPaths
  where 
    extendedAllPaths = extendAllPaths allPaths possibleCaves 

getAllPaths :: [(String,String)] -> [([String],[String],String)]
getAllPaths possibleCaves =  getAllPathsHelper possibleCaves [(["start"],["start"],"")] 

getAllVisualPaths :: [([String],[String],String)] -> [[String]]
getAllVisualPaths [] = []
getAllVisualPaths ((revPath, seenSmallCaves,twice):paths) = (reverse revPath):(getAllVisualPaths paths)
    
main :: IO Int
main = do
  myFile <- openFile "input12.txt" ReadMode
  content <- hGetContents myFile
  return (length (nub (getAllVisualPaths (getAllPaths (getPaths content)))))
