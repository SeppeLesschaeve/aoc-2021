import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getInstruction :: String -> (Int, Int)
getInstruction instruction
  | axis == 'x' = (1, (read value))
  | axis == 'y' = (2, (read value))
  where 
    action = head ( reverse (words instruction))
    axis:'=':value = action 

getInstructions :: [String] -> [(Int,Int)]
getInstructions [] = []
getInstructions (instruction:instructions) = (getInstruction instruction):(getInstructions instructions)    
   
getDots :: [String] -> ([(Int,Int)], [(Int,Int)])
getDots (dotInput:dotsInput)
  | dotInput == "" = ([], (getInstructions dotsInput))
  | otherwise      = ((dotX,dotY):remDots,instruction)
  where 
    dot = splitOn "," dotInput
    dotX = read (dot!!0)
    dotY = read (dot!!1)
    (remDots, instruction) = getDots dotsInput
    
getInputDots :: String -> ([(Int,Int)], [(Int,Int)])
getInputDots inputDots = getDots (lines inputDots)    
    
applyInstruction :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
applyInstruction [] _ = []
applyInstruction ((dotX, dotY):dots) (iden, value)
  | (iden == 1) && (dotX < value) = (dotX,dotY):(applyInstruction dots (iden, value))
  | (iden == 1) && (dotX > value) = ((dotX - (2 * (dotX - value))),dotY):(applyInstruction dots (iden, value))
  | (iden == 2) && (dotY < value) = (dotX,dotY):(applyInstruction dots (iden, value))
  | (iden == 2) && (dotY > value) = (dotX, (dotY - (2 * (dotY - value)))):(applyInstruction dots (iden, value))
  | otherwise                     = applyInstruction dots (iden, value)
  
getDotsAfterInstructionHelper :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
getDotsAfterInstructionHelper dots instruction = nub ( applyInstruction dots instruction)

getDotsAfterInstruction :: ([(Int,Int)], (Int,Int)) -> [(Int,Int)]
getDotsAfterInstruction (dots,instruction) = getDotsAfterInstructionHelper dots instruction

getDotsAfterInstructions :: ([(Int,Int)], [(Int,Int)]) -> [(Int,Int)]
getDotsAfterInstructions (dots, []) = dots
getDotsAfterInstructions (dots, (instruction:instructions)) = getDotsAfterInstructions (newDots, instructions)
  where newDots = getDotsAfterInstruction (dots, instruction)
  
fillRow :: String -> (Int,Int) -> Int -> String
fillRow [] _ _ = []
fillRow (dot:dots) (dotX, dotY) colIndex
  | colIndex == dotX = '*':dots
  | otherwise        = dot:(fillRow dots (dotX,dotY) (colIndex + 1))

fillMatrix :: [String] -> (Int, Int) -> Int -> [String] 
fillMatrix [] _ _ = [] 
fillMatrix (rowOfDots:rowsOfDots) (dotX, dotY) rowIndex 
  | rowIndex == dotY = (fillRow rowOfDots (dotX, dotY) 0):rowsOfDots
  | otherwise        = rowOfDots:(fillMatrix rowsOfDots (dotX, dotY) (rowIndex + 1))      
  
fillAllDots :: [(Int,Int)] -> [String] -> [String]
fillAllDots [] dots = dots
fillAllDots (dot:dots) dotMatrix = fillAllDots dots newDotMatrix
  where newDotMatrix = fillMatrix dotMatrix dot 0  
  
emptyMatrix :: Int -> Int -> [String]
emptyMatrix xMax yMax = replicate (yMax+1) (replicate (xMax+1) ' ')

getMatrixMeasures :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
getMatrixMeasures [] measures = measures
getMatrixMeasures ((dotX,dotY):dots) (curMaxX, curMaxY) = getMatrixMeasures dots ((maximum [dotX, curMaxX]),(maximum [dotY, curMaxY])) 

getFilledMatrix :: [(Int,Int)] -> [String]
getFilledMatrix dots = fillAllDots dots beginMatrix
  where 
    (xMax, yMax) = getMatrixMeasures dots (0,0)
    beginMatrix  = emptyMatrix xMax yMax

printDots :: [(Int,Int)] -> IO ()
printDots dots = mapM_ putStrLn (getFilledMatrix dots)
    
main :: IO ()
main = do
  myFile <- openFile "input13.txt" ReadMode
  content <- hGetContents myFile
  printDots (getDotsAfterInstructions (getInputDots content))
