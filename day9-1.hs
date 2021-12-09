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

getLowPointsRow :: [Int] -> [[Int]] -> Int -> Int -> [Int]
getLowPointsRow [] _ _ _ = []
getLowPointsRow (height:heights) heightMatrix rowIndex colIndex
  | isLowPoint height heightMatrix rowIndex colIndex = height:remainingLowPoints
  | otherwise = remainingLowPoints
  where remainingLowPoints = getLowPointsRow heights heightMatrix rowIndex (colIndex + 1) 

getLowPointsHelper :: [[Int]] -> [[Int]] -> Int -> [Int] -> [Int]
getLowPointsHelper [] _ _ lowPoints = lowPoints
getLowPointsHelper (row:rows) heights rowIndex lowPoints = getLowPointsHelper rows heights (rowIndex + 1) newLowPoints
  where newLowPoints = (getLowPointsRow row heights rowIndex 0) ++ lowPoints   


incrementHeight :: Int -> Int
incrementHeight height = height + 1

getLowPoints :: String -> Int
getLowPoints s = sum ( map incrementHeight lowPoints)
  where 
     heights = getMatrix s
     lowPoints = getLowPointsHelper heights heights 0 []

main :: IO Int
main = do
  myFile <- openFile "input9.txt" ReadMode
  content <- hGetContents myFile
  return (getLowPoints content)
  
