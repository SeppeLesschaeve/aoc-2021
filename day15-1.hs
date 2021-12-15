import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

readRisk :: Char -> Int
readRisk c = read [c]

getRow :: String -> [Int]
getRow [] = []
getRow (risk:risks) = (readRisk risk):(getRow risks) 

getMatrix :: String -> [[Int]]
getMatrix s = map getRow (lines s)

getNeighboursUpperRow :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursUpperRow colIndex m d l r
  | colIndex == 0 = [(0,r), (d,colIndex)]
  | colIndex == m = [(0, l), (d,colIndex)]
  | otherwise = [(0,l), (d, colIndex), (0,r)]

getNeighboursLowerRow :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursLowerRow colIndex m n u l r
  | colIndex == 0 = [(n,r),(u,colIndex)]
  | colIndex == m = [(n, l), (u,colIndex)]
  | otherwise = [(n,l), (u, colIndex), (n,r)]
  
getNeighboursLeftCol :: Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursLeftCol rowIndex u d r = [(u,0),(rowIndex, r),(d,0)]

getNeighboursRightCol :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursRightCol rowIndex m u d l = [(u,m),(rowIndex, l),(d,m)]  

getNeighbours :: (Int,Int) -> [[Int]] -> [(Int,Int)]
getNeighbours (rowIndex, colIndex) risks
  | rowIndex == 0 = getNeighboursUpperRow colIndex m d l r
  | rowIndex == n = getNeighboursLowerRow colIndex m n u l r
  | colIndex == 0 = getNeighboursLeftCol rowIndex u d r
  | colIndex == m = getNeighboursRightCol rowIndex m u d l
  | otherwise     = [(u,colIndex),(rowIndex,l),(rowIndex,r),(d,colIndex)]
  where 
    m = (length risks) - 1
    n = (length risks) - 1 
    u = rowIndex - 1
    d = rowIndex + 1
    l = colIndex - 1
    r = colIndex + 1

maxRisk :: Int -> Int
maxRisk n = 10 * n * n

markDistanceRisk :: [Int] -> Int -> Int -> Int -> [Int]
markDistanceRisk (risk:risks) colIndex riskM amount
  | colIndex == riskM = amount:risks
  | otherwise         = risk:(markDistanceRisk risks (colIndex + 1) riskM amount)
    
markDistance :: [[Int]] -> Int -> (Int,Int) -> Int -> [[Int]]
markDistance (row:rows) rowIndex (riskN, riskM) amount
  | rowIndex == riskN = (markDistanceRisk row 0 riskM amount):rows
  | otherwise         = row:(markDistance rows (rowIndex + 1) (riskN, riskM) amount)

markNeighbours :: [(Int,Int)] -> [[Int]] -> [[Bool]] -> [[Int]] -> (Int, Int) -> [[Int]]
markNeighbours [] _ _ distance _ = distance 
markNeighbours ((neighX, neighY):neighbours) risks visited distance (rowIndex, colIndex)
  | condition == True = markNeighbours neighbours risks visited newDistance (rowIndex, colIndex)
  | otherwise         = markNeighbours neighbours risks visited distance    (rowIndex, colIndex)
  where 
    distanceNeighbour    = (distance!!neighX!!neighY)
    newDistanceNeighbour = (risks!!neighX!!neighY) + (distance!!rowIndex!!colIndex)
    condition  = (visited!!neighX!!neighY == False) && (distance!!rowIndex!!colIndex < (maxRisk (length visited))) && (newDistanceNeighbour < distanceNeighbour)
    newDistance = markDistance distance 0 (neighX, neighY) newDistanceNeighbour

getInitialDistanceRow :: [Int] -> Int -> Int -> Int -> [Int]
getInitialDistanceRow (risk:risks) rowIndex colIndex n
  | (rowIndex == 0) && (colIndex == 0) = 0:(getInitialDistanceRow risks rowIndex (colIndex + 1) n)
  | otherwise                          = map (\x -> n) (risk:risks)

getInitialDistanceMatrix :: [[Int]] -> Int -> Int -> [[Int]]
getInitialDistanceMatrix [] _ _ = []
getInitialDistanceMatrix (row:rows) rowIndex n = initialDistanceRow:initialDistanceRows
  where 
    initialDistanceRow = getInitialDistanceRow row rowIndex 0 n
    initialDistanceRows = getInitialDistanceMatrix rows (rowIndex + 1) n

markVisitedRisk :: [Bool] -> Int -> Int -> [Bool]
markVisitedRisk (risk:risks) colIndex riskM 
  | colIndex == riskM = True:risks
  | otherwise         = risk:(markVisitedRisk risks (colIndex + 1) riskM)
    
markVisited :: [[Bool]] -> Int -> (Int,Int) -> [[Bool]]
markVisited (row:rows) rowIndex (riskN, riskM)
  | rowIndex == riskN = (markVisitedRisk row 0 riskM):rows
  | otherwise         = row:(markVisited rows (rowIndex + 1) (riskN, riskM)) 

getInitialVisitedRow :: [Int] -> [Bool]
getInitialVisitedRow = map (\x -> False)

getInitialVisitedMatrix :: [[Int]] -> [[Bool]]
getInitialVisitedMatrix = map (getInitialVisitedRow)

getMinimalDistanceHelper :: [Int] -> [[Bool]] -> Int -> Int -> (Int, Int, Int) -> (Int, Int, Int)
getMinimalDistanceHelper [] _ _ _ (rowIndex, colIndex, d) = (rowIndex, colIndex, d)
getMinimalDistanceHelper (risk:risks) visited riskN riskM (rowIndex, colIndex, d)
  | condition == True = getMinimalDistanceHelper risks visited riskN (riskM + 1) (riskN, riskM, risk)
  | otherwise         = getMinimalDistanceHelper risks visited riskN (riskM + 1) (rowIndex, colIndex, d)
  where 
    condition = (risk < d) && (visited!!(riskN)!!(riskM) == False)

getMinimalDistance :: [[Int]] -> [[Bool]] -> Int -> (Int, Int, Int) -> (Int, Int, Int)
getMinimalDistance [] _ _ (rowIndex, colIndex, d) = (rowIndex, colIndex , d)
getMinimalDistance (distance:distances) visited riskN (rowIndex, colIndex, d) = newMinimal
  where 
    (riskI, riskJ, riskD) = getMinimalDistanceHelper distance visited riskN 0 (rowIndex, colIndex, d)
    newMinimal            = getMinimalDistance distances visited (riskN + 1) (riskI, riskJ, riskD)

findLstRiskHelper :: Int -> [[Int]] -> [[Int]] -> [[Bool]] -> Int
findLstRiskHelper 0 _ distance _ = distance!!(length distance - 1)!!(length distance - 1)
findLstRiskHelper n risks distance visited = findLstRiskHelper (n-1) risks newDistance newVisited
  where 
    m = length risks
    (rowIndex, colIndex, d) = getMinimalDistance distance visited 0 (-1,-1,(maxRisk m))
    newVisited = markVisited visited 0 (rowIndex, colIndex)
    neighbours = getNeighbours (rowIndex, colIndex) distance
    newDistance = markNeighbours neighbours risks newVisited distance (rowIndex, colIndex)

findLstRisk :: [[Int]] -> Int
findLstRisk risks = findLstRiskHelper ((length risks) * (length risks)) risks distance visited 
  where 
    distance = getInitialDistanceMatrix risks 0 (maxRisk (length risks))
    visited   = getInitialVisitedMatrix risks
    
main :: IO Int
main = do
  myFile <- openFile "input15.txt" ReadMode
  content <- hGetContents myFile
  return (findLstRisk (getMatrix content)) 

    
