import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

readEnergy :: Char -> Int
readEnergy c = read [c]

getRow :: String -> [Int]
getRow [] = []
getRow (energy:energys) = (readEnergy energy):(getRow energys) 

getMatrix :: String -> [[Int]]
getMatrix s = map getRow (lines s)

increaseMatrix :: [[Int]] -> [[Int]]
increaseMatrix [] = []
increaseMatrix (row:rows) = (map (+1) row):(increaseMatrix rows)

getFlashesRow :: [Int] -> Int -> Int -> [(Int,Int)]
getFlashesRow [] _ _ = []
getFlashesRow (cell:cells) rowIndex colIndex
  | cell > 9 = (rowIndex, colIndex):remainingFlashes
  | otherwise = remainingFlashes
  where remainingFlashes = getFlashesRow cells rowIndex (colIndex + 1)

getFlashes :: [[Int]] -> Int -> [(Int,Int)]
getFlashes [] _ = []
getFlashes (row:rows) rowIndex = (getFlashesRow row rowIndex 0) ++ (getFlashes rows (rowIndex + 1))

getNeighboursUpperRow :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursUpperRow colIndex m d l r
  | colIndex == 0 = [(0,r),(d,r), (d,colIndex)]
  | colIndex == m = [(0, l), (d, l), (d,colIndex)]
  | otherwise = [(0,l), (d,l), (d, colIndex), (d, r), (0,r)]

getNeighboursLowerRow :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursLowerRow colIndex m n u l r
  | colIndex == 0 = [(n,r),(u,r),(u,colIndex)]
  | colIndex == m = [(n, l), (u, l), (u,colIndex)]
  | otherwise = [(n,l), (u,l), (u, colIndex), (u, r), (n,r)]
  
getNeighboursLeftCol :: Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursLeftCol rowIndex u d r = [(u,0),(u,r),(rowIndex, r), (d, r),(d,0)]

getNeighboursRightCol :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)]
getNeighboursRightCol rowIndex m u d l = [(u,m),(u,l),(rowIndex, l), (d, l),(d,m)]  

getNeighbours :: (Int,Int) -> [[Int]] -> [(Int,Int)]
getNeighbours (rowIndex, colIndex) flashMatrix
  | rowIndex == 0 = getNeighboursUpperRow colIndex m d l r
  | rowIndex == n = getNeighboursLowerRow colIndex m n u l r
  | colIndex == 0 = getNeighboursLeftCol rowIndex u d r
  | colIndex == m = getNeighboursRightCol rowIndex m u d l
  | otherwise     = [(u,l),(u,colIndex),(u,r),(rowIndex,l),(rowIndex,r),(d,l),(d,colIndex),(d,r)]
  where 
    m = (length (head flashMatrix)) - 1
    n = (length flashMatrix) - 1 
    u = rowIndex - 1
    d = rowIndex + 1
    l = colIndex - 1
    r = colIndex + 1

neutralizeFlashRow :: [Int] -> Int -> Int -> [Int]
neutralizeFlashRow (cell:cells) currentColIndex colIndex 
  | currentColIndex == colIndex = 0:cells
  | otherwise                   = cell:(neutralizeFlashRow cells (currentColIndex + 1) colIndex)

neutralizeFlashMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
neutralizeFlashMatrix (row:rows) currentRowIndex rowIndex colIndex
  | currentRowIndex == rowIndex = (neutralizeFlashRow row 0 colIndex):rows
  | otherwise                   = row:(neutralizeFlashMatrix rows (currentRowIndex + 1) rowIndex colIndex) 

neutralizeFlashing :: (Int, Int) -> [[Int]] -> [[Int]]
neutralizeFlashing (rowIndex,colIndex) flashMatrix = neutralizeFlashMatrix flashMatrix 0 rowIndex colIndex

increaseFlashRow :: [Int] -> Int -> Int -> [Int]
increaseFlashRow (cell:cells) currentColIndex colIndex 
  | currentColIndex == colIndex = (cell + 1):cells
  | otherwise                   = cell:(increaseFlashRow cells (currentColIndex + 1) colIndex)

increaseFlashMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
increaseFlashMatrix (row:rows) currentRowIndex rowIndex colIndex
  | currentRowIndex == rowIndex = (increaseFlashRow row 0 colIndex):rows
  | otherwise                   = row:(increaseFlashMatrix rows (currentRowIndex + 1) rowIndex colIndex) 

increaseNeighbours :: [(Int, Int)] -> [[Int]] -> [[Int]]
increaseNeighbours [] flashMatrix = flashMatrix
increaseNeighbours ((rowIndex,colIndex):neighbours) flashMatrix = increaseNeighbours neighbours newFlashMatrix
  where newFlashMatrix = increaseFlashMatrix flashMatrix 0 rowIndex colIndex

getFlashingHelper :: [(Int, Int)] -> [(Int,Int)] -> [[Int]] -> [(Int,Int)]
getFlashingHelper [] _ _ = []
getFlashingHelper ((rowIndex,colIndex):neighbours) currentFlashing flashMatrix
  | ((flashMatrix!!rowIndex!!colIndex) > 9) && (not (elem (rowIndex,colIndex) currentFlashing)) = (rowIndex,colIndex):remaining
  | otherwise = remaining
  where remaining = getFlashingHelper neighbours currentFlashing flashMatrix  
    
getFlashingNeighbours :: (Int, Int) -> [(Int,Int)] -> [[Int]] -> [(Int,Int)]
getFlashingNeighbours flashing currentFlashing flashMatrix = getFlashingHelper (getNeighbours flashing flashMatrix) currentFlashing flashMatrix 

getNoDupsNewFlashes :: [(Int,Int)] -> [(Int,Int)]
getNoDupsNewFlashes = map head . group . sort 

trackFlashes :: [(Int,Int)] -> [(Int,Int)]  -> [[Int]] -> (Int,[[Int]])
trackFlashes [] _ flashMatrix = (0,flashMatrix)
trackFlashes (flashing:flashings) seenFlashings flashMatrix = ((1 + newAmount), subFlashMatrix)
  where 
    neighbours = getNeighbours flashing flashMatrix
    flashMatrix1 = increaseNeighbours neighbours flashMatrix
    newFlashings    = getNoDupsNewFlashes ((getFlashingNeighbours flashing seenFlashings flashMatrix1) ++ flashings)
    (newAmount, flashMatrix2) = trackFlashes newFlashings (flashing:seenFlashings) flashMatrix1
    subFlashMatrix = neutralizeFlashing flashing flashMatrix2  
    
trackFlashes100 :: Int -> [[Int]] -> Int -> Int
trackFlashes100 0 _ score = score
trackFlashes100 n flashMatrix score = trackFlashes100 (n-1) subFlashMatrix (score + newAmount)
  where 
    increasedMatrix = increaseMatrix flashMatrix
    flashings  =  getFlashes increasedMatrix 0
    (newAmount, subFlashMatrix) = trackFlashes flashings [] increasedMatrix
    
main :: IO Int
main = do
  myFile <- openFile "input11.txt" ReadMode
  content <- hGetContents myFile
  return (trackFlashes100 100 (getMatrix content) 0)      
