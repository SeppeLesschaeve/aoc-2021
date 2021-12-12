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

extendPathStop :: (String, String) -> ([String],[String]) -> [([String],[String])]
extendPathStop (begin,stop) (path,seenSmallCaves)
  | (not (isSmallCave stop))         = [(stop:path, seenSmallCaves)]
  | (not (elem stop seenSmallCaves)) = [(stop:path, stop:seenSmallCaves)]
  | otherwise                        = []

extendPathBegin :: (String, String) -> ([String],[String]) -> [([String],[String])]
extendPathBegin (begin,stop) (path,seenSmallCaves)
  | (not (isSmallCave begin))         = [(begin:path, seenSmallCaves)] 
  | (not (elem begin seenSmallCaves)) = [(begin:path, begin:seenSmallCaves)]
  | otherwise                         = []

extendPath :: [(String, String)] -> ([String],[String]) -> [([String],[String])]
extendPath [] path = []
extendPath ((begin,stop):possibleCaves) (path,seenSmallCaves)
  | (head path) == begin = (extendPathStop (begin,stop) (path,seenSmallCaves)) ++ remainingExtend
  | (head path) == stop  = (extendPathBegin (begin,stop) (path,seenSmallCaves)) ++ remainingExtend
  | otherwise            = remainingExtend
  where remainingExtend = extendPath possibleCaves (path,seenSmallCaves)

extendAllPaths :: [([String],[String])] -> [(String, String)] -> [([String],[String])]
extendAllPaths [] _ = []
extendAllPaths (path:paths) possibleCaves
  | (head currPath) == "end" = path:(extendAllPaths paths possibleCaves)
  | otherwise                = (extendPath possibleCaves path) ++ (extendAllPaths paths possibleCaves)
  where 
    (currPath, currSeenSmallCaves) = path

areAllEnding :: [([String],[String])] -> Bool
areAllEnding [] = True
areAllEnding ((path,seenSmallCaves):paths) 
  | (head path) == "end" = areAllEnding paths
  | otherwise            = False 
    
getAllPathsHelper :: [(String,String)] -> [([String],[String])] -> [([String],[String])]   
getAllPathsHelper possibleCaves allPaths 
  | areAllEnding allPaths = allPaths
  | otherwise             = getAllPathsHelper possibleCaves extendedAllPaths
  where 
    extendedAllPaths = extendAllPaths allPaths possibleCaves 

getAllPaths :: [(String,String)] -> [([String],[String])]
getAllPaths possibleCaves = getAllPathsHelper possibleCaves [(["start"],["start"])] 
    
main :: IO Int
main = do
  myFile <- openFile "input12.txt" ReadMode
  content <- hGetContents myFile
  return (length (getAllPaths (getPaths content)))
