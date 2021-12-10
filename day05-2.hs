import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getCoordinate :: String -> (Int,Int)
getCoordinate ventLineList = (coordinate!!0, coordinate!!1)
   where coordinate = map read (splitOn "," ventLineList)

getVentLine :: String -> ((Int,Int), (Int,Int))
getVentLine ventLine = ((getCoordinate (ventLineList!!0)),(getCoordinate (ventLineList!!2)))
   where 
      ventLineList = words ventLine

getVentLines :: String -> [((Int,Int), (Int,Int))]
getVentLines s = map getVentLine (lines s)

getXDim :: [((Int,Int), (Int,Int))] -> Int -> Int
getXDim [] xDim = xDim
getXDim (ventLine:ventLines) currentXDim = getXDim ventLines (maximum [currentXDim,x1,x2])
  where ((x1,y1),(x2,y2)) = ventLine

getYDim :: [((Int,Int), (Int,Int))] -> Int -> Int
getYDim [] yDim = yDim
getYDim (ventLine:ventLines) currentYDim = getYDim ventLines (maximum [currentYDim,y1,y2])
  where ((x1,y1),(x2,y2)) = ventLine

emptyVentLineRow :: Int -> Int -> [(Int, Int, Int)]
emptyVentLineRow 0 n = [(n,0,0)]
emptyVentLineRow m n = (emptyVentLineRow (m - 1) n) ++ [(n,m,0)]
  
emptyVentLineList :: Int -> Int -> [[(Int,Int,Int)]]
emptyVentLineList 0 _ = []
emptyVentLineList n m = (emptyVentLineRow m n):(emptyVentLineList (n - 1) m)

markRowVentLine :: [(Int,Int,Int)] -> Int -> Int -> [(Int,Int,Int)]
markRowVentLine [] _ _ = []
markRowVentLine ((x,y,o):places) yBegin yEnd
  | (y >= yBegin) && (y <= yEnd) = (x,y,o + 1):(markRowVentLine places yBegin yEnd)
  | otherwise = (x,y,o):(markRowVentLine places yBegin yEnd)

markVentLineHorizontal :: [[(Int,Int,Int)]] -> ((Int,Int), (Int,Int)) -> [[(Int,Int,Int)]] 
markVentLineHorizontal [] _ = []
markVentLineHorizontal (row:rows) ((x1,y1),(x2,y2))
  | x1 == x   = (markRowVentLine row yBegin yEnd):rows
  | otherwise = row:(markVentLineHorizontal rows ((x1,y1),(x2,y2)))  
  where
    (x,y,o) = head row  
    yBegin = minimum [y1,y2]
    yEnd   = maximum [y1,y2]
    
markColVentLine :: [(Int,Int,Int)] -> Int -> [(Int,Int,Int)]
markColVentLine [] _ = []
markColVentLine ((x,y,o): places) col
  | y == col = (x,y,o + 1):places
  | otherwise = (x,y,o):(markColVentLine places col)

markVentLineVertical :: [[(Int,Int,Int)]] -> ((Int,Int), (Int,Int)) -> [[(Int,Int,Int)]] 
markVentLineVertical [] _ = []
markVentLineVertical (row:rows) ((x1,y1),(x2,y2))
  | (x >= xBegin) && (x <= xEnd) = (markColVentLine row y1):remainingMark
  | otherwise              = row:remainingMark
  where 
    (x,y,o) = head row
    xBegin = minimum [x1,x2]
    xEnd   = maximum [x1,x2]
    remainingMark = (markVentLineVertical rows ((x1,y1),(x2,y2))) 
  
getAllDiagonalElems :: ( (Int,Int) , (Int,Int)) -> [(Int,Int)]
getAllDiagonalElems ((x1,y1),(x2,y2)) 
  | (x1 == x2) && (y1 == y2) = [(x1,y1)]
  | x1 > x2 = getAllDiagonalElems ((x2,y2),(x1,y1))
  | (x1 < x2) && (y1 < y2) = (x1,y1):(getAllDiagonalElems ((x1+1,y1+1),(x2,y2)))
  | (x1 < x2) && (y1 > y2) = (x1,y1):(getAllDiagonalElems ((x1+1,y1-1),(x2,y2)))

markVentLineDiagonal :: [(Int,Int)] -> [[(Int,Int,Int)]] ->[[(Int,Int,Int)]] 
markVentLineDiagonal [] ventLineList = ventLineList
markVentLineDiagonal ((x,y):diagElems) ventLineList = markVentLineDiagonal diagElems newVentLineList
  where
    newVentLineList = markVentLineHorizontal ventLineList ((x,y),(x,y)) 
  
isDiagonal :: ((Int,Int), (Int,Int)) -> Bool
isDiagonal ((x1,y1),(x2,y2)) = abs (x2 - x1) == abs (y2 - y1)
  
markVentLine :: ((Int,Int), (Int,Int)) -> [[(Int,Int,Int)]] -> [[(Int,Int,Int)]]
markVentLine ((x1,y1),(x2,y2)) ventLineList 
  | isDiagonal ((x1,y1),(x2,y2)) = markVentLineDiagonal (getAllDiagonalElems ((x1,y1),(x2,y2))) ventLineList
  | x1 == x2 = markVentLineHorizontal ventLineList ((x1,y1),(x2,y2))
  | y1 == y2 = markVentLineVertical ventLineList ((x1,y1),(x2,y2))
  | otherwise = ventLineList

countMultipleOverlapsRow :: [(Int,Int,Int)] -> Int
countMultipleOverlapsRow [] = 0
countMultipleOverlapsRow ((x,y,o):places) 
  | o >= 2 = 1 + (countMultipleOverlapsRow places)
  | otherwise  = (countMultipleOverlapsRow places)
  
countMultipleOverlaps :: [[(Int,Int,Int)]] -> Int
countMultipleOverlaps [] = 0
countMultipleOverlaps (row:rows) = (countMultipleOverlapsRow row) + (countMultipleOverlaps rows)  
  
getMultipleOverlapsHelper :: [((Int,Int), (Int,Int))] -> [[(Int,Int,Int)]] -> Int
getMultipleOverlapsHelper [] ventLineList = countMultipleOverlaps ventLineList
getMultipleOverlapsHelper (ventLine:ventLines) ventLineList = getMultipleOverlapsHelper ventLines newVentLineList 
  where newVentLineList = markVentLine ventLine ventLineList
  
getMultipleOverlaps :: [((Int,Int), (Int,Int))] -> Int
getMultipleOverlaps ventLines = getMultipleOverlapsHelper ventLines (emptyVentLineList (getXDim ventLines 0) (getYDim ventLines 0) ) 

main :: IO Int
main = do
  myFile <- openFile "input05.txt" ReadMode
  content <- hGetContents myFile
  return (getMultipleOverlaps (getVentLines content))
  
