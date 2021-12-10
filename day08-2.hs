import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getBuckets :: [String] -> [[String]] -> [[String]]
getBuckets [] buckets = buckets
getBuckets (codedInput:codedInputs) (b1:b2:b3:b4:b5:b6:[])
  | (length codedInput) == 2 = getBuckets codedInputs ((codedInput:b1):b2:b3:b4:b5:b6:[])
  | (length codedInput) == 3 = getBuckets codedInputs (b1:(codedInput:b2):b3:b4:b5:b6:[])
  | (length codedInput) == 4 = getBuckets codedInputs (b1:b2:(codedInput:b3):b4:b5:b6:[])
  | (length codedInput) == 5 = getBuckets codedInputs (b1:b2:b3:(codedInput:b4):b5:b6:[])
  | (length codedInput) == 6 = getBuckets codedInputs (b1:b2:b3:b4:(codedInput:b5):b6:[])
  | (length codedInput) == 7 = getBuckets codedInputs (b1:b2:b3:b4:b5:(codedInput:b6):[])

getCommonChars :: [String] -> [Char] -> [Char]
getCommonChars [] commonChars = commonChars
getCommonChars (codedInput:codedInputs) commonChars = getCommonChars codedInputs newCommonChars
  where newCommonChars = intersect commonChars codedInput 

getMappingHelper :: [[String]] -> [(Char,Char)]
getMappingHelper buckets = [(mappedA, 'a'),(mappedB, 'b'),(mappedC,'c'),(mappedD,'d'),(mappedE, 'e'),(mappedF, 'f'),(mappedG, 'g')] 
  where
    cpair = buckets!!0!!0
    fpair = buckets!!0!!0
    mappedA = head ((buckets!!1!!0)\\cpair)
    bpair = (buckets!!2!!0)\\(cpair)
    dpair = (buckets!!2!!0)\\(cpair)
    commonCodedChars = getCommonChars (buckets!!4) (buckets!!4!!0)
    mappedB = head (intersect bpair commonCodedChars)
    mappedF = head (intersect fpair commonCodedChars)
    mappedC = head (cpair\\[mappedF])
    mappedD = head (dpair\\[mappedB])
    mappedG = head (commonCodedChars\\[mappedA, mappedB, mappedF])
    mappedE = head (['a','b','c','d','e','f','g']\\[mappedA, mappedB, mappedC, mappedD, mappedF, mappedG])
    
getMapping :: [String] -> [(Char, Char)]
getMapping codedInputs = getMappingHelper (getBuckets codedInputs [[],[],[],[],[],[]])

getCodedInputsHelper :: [String] -> [([String], [String])]
getCodedInputsHelper [] = []
getCodedInputsHelper (digit:digits) = (codedInput, codedOutput):(getCodedInputsHelper digits)
   where 
      allDigits = (splitOn " | " digit)
      codedInput = words (allDigits!!0)
      codedOutput = words (allDigits!!1)

getRealChar :: [(Char,Char)] -> Char -> Char
getRealChar ((codedChar,realChar):mapping) currentChar
  | codedChar == currentChar = realChar
  | otherwise = getRealChar mapping currentChar

getRealChars :: String -> [(Char,Char)] -> [Char]
getRealChars [] _ = []
getRealChars (codedChar:codedChars) mapping = (getRealChar mapping codedChar):(getRealChars codedChars mapping)

is0 :: [Char] -> Bool
is0 chars = (elem 'a' chars) && (elem 'b' chars) && (elem 'c' chars) && (not (elem 'd' chars)) && (elem 'e' chars) && (elem 'f' chars) && (elem 'g' chars) 

is1 :: [Char] -> Bool
is1 chars = (not (elem 'a' chars)) && (not (elem 'b' chars)) && (elem 'c' chars) && (not (elem 'd' chars)) && (not (elem 'e' chars)) && (elem 'f' chars) && (not(elem 'g' chars)) 

is2 :: [Char] -> Bool
is2 chars = (elem 'a' chars) && (not (elem 'b' chars)) && (elem 'c' chars) && (elem 'd' chars) && (elem 'e' chars) && (not(elem 'f' chars)) && (elem 'g' chars) 

is3 :: [Char] -> Bool
is3 chars = (elem 'a' chars) && (not (elem 'b' chars)) && (elem 'c' chars) && (elem 'd' chars) && (not (elem 'e' chars)) && (elem 'f' chars) && (elem 'g' chars) 

is4 :: [Char] -> Bool
is4 chars = (not (elem 'a' chars)) && (elem 'b' chars) && (elem 'c' chars) && (elem 'd' chars) && (not (elem 'e' chars)) && (elem 'f' chars) && (not (elem 'g' chars)) 

is5 :: [Char] -> Bool
is5 chars = (elem 'a' chars) && (elem 'b' chars) && (not (elem 'c' chars)) && (elem 'd' chars) && (not (elem 'e' chars)) && (elem 'f' chars) && (elem 'g' chars) 

is6 :: [Char] -> Bool
is6 chars = (elem 'a' chars) && (elem 'b' chars) && (not (elem 'c' chars)) && (elem 'd' chars) && (elem 'e' chars) && (elem 'f' chars) && (elem 'g' chars) 

is7 :: [Char] -> Bool
is7 chars = (elem 'a' chars) && (not (elem 'b' chars)) && (elem 'c' chars) && (not (elem 'd' chars)) && (not (elem 'e' chars)) && (elem 'f' chars) && (not (elem 'g' chars)) 

is8 :: [Char] -> Bool
is8 chars = (elem 'a' chars) && (elem 'b' chars) && (elem 'c' chars) && (elem 'd' chars) && (elem 'e' chars) && (elem 'f' chars) && (elem 'g' chars) 

is9 :: [Char] -> Bool
is9 chars = (elem 'a' chars) && (elem 'b' chars) && (elem 'c' chars) && (elem 'd' chars) && (not (elem 'e' chars)) && (elem 'f' chars) && (elem 'g' chars) 

getRealOutputHelper :: [Char] -> Int
getRealOutputHelper chars
  | is0 chars = 0
  | is1 chars = 1
  | is2 chars = 2
  | is3 chars = 3
  | is4 chars = 4
  | is5 chars = 5
  | is6 chars = 6
  | is7 chars = 7
  | is8 chars = 8
  | is9 chars = 9
  
      
getRealOutput :: [String] -> [(Char, Char)] -> Int -> Int
getRealOutput [] _ _ = 0
getRealOutput (codedOutput:codedOutputs) mapping power = (realOutput * power) + (getRealOutput codedOutputs mapping (div power 10))
  where realOutput = getRealOutputHelper (getRealChars codedOutput mapping)           

getSumOfOutputsHelper :: [([String], [String])] -> Int -> Int
getSumOfOutputsHelper [] sumOfOutputs = sumOfOutputs
getSumOfOutputsHelper ((codedInputs,codedOutputs):codedSignals) sumOfOutputs = getSumOfOutputsHelper codedSignals (sumOfOutputs + realOutput)
  where 
    mapping = getMapping codedInputs
    realOutput = getRealOutput codedOutputs mapping 1000

getSumOfOutputs :: String -> Int 
getSumOfOutputs s = getSumOfOutputsHelper (getCodedInputsHelper (lines s)) 0

main :: IO Int
main = do
  myFile <- openFile "input08.txt" ReadMode
  content <- hGetContents myFile
  return (getSumOfOutputs content)  
