import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

readHeight :: Char -> Int
readHeight c = read [c]

getRow :: String -> [Int]
getRow [] = []
getRow (height:heights) = (readHeight height):(getRow heights) 

getMatrix :: String -> [[Int]]
getMatrix s = map getRow (lines s)

isLowPointUpperRow :: Int -> [[Int]] -> Int -> Int -> Bool
isLowPointUpperRow height heightMatrix colIndex m
  | colIndex == 0 = (height < (heightMatrix!!1!!0)) && (height < (heightMatrix!!0!!1))
  | colIndex == m = (height < (heightMatrix!!1!!m)) && (height < (heightMatrix!!0!!(m-1)))
  | otherwise = (height < (heightMatrix!!1!!colIndex)) && (height < (heightMatrix!!0!!(colIndex-1))) && (height < (heightMatrix!!0!!(colIndex+1)))
  
isLowPointLowerRow :: Int -> [[Int]] -> Int -> Int -> Int -> Bool
isLowPointLowerRow height heightMatrix colIndex m n
  | colIndex == 0 = (height < (heightMatrix!!(n-1)!!0)) && (height < (heightMatrix!!n!!1))
  | colIndex == m = (height < (heightMatrix!!(n-1)!!m)) && (height < (heightMatrix!!n!!(m-1)))
  | otherwise = (height < (heightMatrix!!(n-1)!!colIndex)) && (height < (heightMatrix!!n!!(colIndex-1))) && (height < (heightMatrix!!n!!(colIndex+1)))
  
isLowPointLeftCol :: Int -> [[Int]] -> Int -> Bool
isLowPointLeftCol height heightMatrix rowIndex = (height < (heightMatrix!!rowIndex!!1)) && (height < (heightMatrix!!(rowIndex - 1)!!0)) && (height < (heightMatrix!!(rowIndex + 1)!!0))  

isLowPointRightCol :: Int -> [[Int]] -> Int -> Int -> Bool
isLowPointRightCol height heightMatrix rowIndex m = (height < (heightMatrix!!rowIndex!!(m-1))) && (height < (heightMatrix!!(rowIndex - 1)!!m)) && (height < (heightMatrix!!(rowIndex + 1)!!m))  

isLowPoint :: Int -> [[Int]] -> Int -> Int -> Bool
isLowPoint height heightMatrix rowIndex colIndex
  | rowIndex == 0 = isLowPointUpperRow height heightMatrix colIndex m
  | rowIndex == n = isLowPointLowerRow height heightMatrix colIndex m n
  | colIndex == 0 = isLowPointLeftCol  height heightMatrix rowIndex
  | colIndex == m = isLowPointRightCol height heightMatrix rowIndex m
  | otherwise     = (height < (heightMatrix!!u!!colIndex)) && (height < (heightMatrix!!d!!colIndex)) && (height < (heightMatrix!!rowIndex!!l)) && (height < (heightMatrix!!rowIndex!!r))
  where 
    m = (length (head heightMatrix)) - 1
    n = (length heightMatrix) - 1 
    u = rowIndex - 1
    d = rowIndex + 1
    l = colIndex - 1
    r = colIndex + 1

getLowPointsRow :: [Int] -> [[Int]] -> Int -> Int -> [(Int,Int,Int)]
getLowPointsRow [] _ _ _ = []
getLowPointsRow (height:heights) heightMatrix rowIndex colIndex
  | isLowPoint height heightMatrix rowIndex colIndex = (rowIndex,colIndex,height):remainingLowPoints
  | otherwise = remainingLowPoints
  where remainingLowPoints = getLowPointsRow heights heightMatrix rowIndex (colIndex + 1) 

getLowPointsHelper :: [[Int]] -> [[Int]] -> Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
getLowPointsHelper [] _ _ lowPoints = lowPoints
getLowPointsHelper (row:rows) heights rowIndex lowPoints = getLowPointsHelper rows heights (rowIndex + 1) newLowPoints
  where newLowPoints = (getLowPointsRow row heights rowIndex 0) ++ lowPoints

getBassin :: [[Int]] -> [(Int, Int)] -> Int -> Int -> Int -> Int -> (Int, [(Int,Int)])
getBassin heightMatrix seenHeights n m rowIndex colIndex 
  | (rowIndex < 0) || (rowIndex >= (n+1)) || (colIndex < 0) || (colIndex >= (m+1)) || (elem (rowIndex, colIndex) seenHeights) || (heightMatrix!!rowIndex!!colIndex == 9) = (0,seenHeights)
  | otherwise = (1 + rightSize + leftSize + upperSize + lowerSize, newSeenLower)
  where 
    newSeenHeights = (rowIndex, colIndex):seenHeights
    (rightSize,newSeenRight) = getBassin heightMatrix newSeenHeights n m (rowIndex + 1) colIndex
    (leftSize,newSeenLeft) = getBassin heightMatrix newSeenRight n m (rowIndex - 1) colIndex
    (upperSize,newSeenUpper) = getBassin heightMatrix newSeenLeft n m rowIndex (colIndex - 1)
    (lowerSize,newSeenLower) = getBassin heightMatrix newSeenUpper n m rowIndex (colIndex + 1)


getBassins :: [(Int,Int,Int)] -> [[Int]] -> [(Int, Int)] -> [Int]
getBassins [] _ _ = []
getBassins ((lowX, lowY, lowHeight):lowPoints) heightMatrix seenHeights = (bassinSize):(getBassins lowPoints heightMatrix newSeenHeights) 
  where  
    (bassinSize, newSeenHeights) = getBassin heightMatrix seenHeights n m lowX lowY 
    m = (length (head heightMatrix)) - 1
    n = (length heightMatrix) - 1

getMultiplyOfThreeBassins :: String -> Int
getMultiplyOfThreeBassins s = (bassins!!0) * (bassins!!1) * (bassins!!2)  
  where 
     heightMatrix = getMatrix s
     m = (length (head heightMatrix)) - 1
     n = (length heightMatrix) - 1
     lowPoints = getLowPointsHelper heightMatrix heightMatrix 0 []
     bassins = sortBy (flip compare) (getBassins lowPoints heightMatrix [])

main :: IO Int
main = do
  myFile <- openFile "input9.txt" ReadMode
  content <- hGetContents myFile
  return (getMultiplyOfThreeBassins content)
