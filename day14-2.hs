import System.IO  
import Control.Monad
import Data.List.Split
import Data.List
import Data.Ord

getPolymerRules :: [String] -> [(Char,Char,Char)]
getPolymerRules [] = []
getPolymerRules (rule:rules) = [(firstPoly, secondPoly, inserting)] ++ (getPolymerRules rules)
   where 
      allPolymers = (splitOn " -> " rule)
      leftPattern = allPolymers!!0
      firstPoly   = leftPattern!!0
      secondPoly  = leftPattern!!1
      inserting   = head (allPolymers!!1)
            
getPolymerInput :: String -> (String, [(Char,Char,Char)])
getPolymerInput s = ((head polyLines), (getPolymerRules (tail (tail (polyLines)))))
  where polyLines = lines s

getEmptyBuckets :: [(Char,Char,Char)] -> [(Char,Char,Integer)]
getEmptyBuckets [] = []
getEmptyBuckets ((c1,c2,insert):rules) = [(c1,c2,0)] ++ (getEmptyBuckets rules)

fillBucket :: [(Char,Char,Integer)] -> (Char, Char) -> [(Char,Char,Integer)]
fillBucket [] _ = []
fillBucket ((firstPoly,secondPoly,amount):buckets) (c1,c2)
  | (firstPoly == c1) && (secondPoly == c2) = (firstPoly,secondPoly,(amount + 1)):buckets
  | otherwise                               = (firstPoly,secondPoly,amount):(fillBucket buckets (c1,c2))
  
fillBuckets :: String -> [(Char,Char,Integer)] -> [(Char,Char,Integer)]
fillBuckets (c:[]) buckets = buckets
fillBuckets (c1:c2:s) buckets = fillBuckets (c2:s) (fillBucket buckets (c1,c2))  

isInOccurrences :: [(Char,Integer)] -> Char -> Bool
isInOccurrences [] _ = False
isInOccurrences ((c1,amount):occurrences) c2 
  | c1 == c2 = True
  | otherwise = isInOccurrences occurrences c2 

getEmptyOccurrences :: String -> [(Char,Integer)] -> [(Char,Integer)]
getEmptyOccurrences [] occurrences = occurrences
getEmptyOccurrences (c1:s) occurrences 
  | (isInOccurrences occurrences c1) || (c1 == ' ') || (c1 == '\n') || (c1 == '-') || (c1 == '>') = getEmptyOccurrences s occurrences
  | otherwise                        = getEmptyOccurrences s ([(c1,0)] ++ occurrences)

fillOccurrence :: [(Char,Integer)] -> Char -> [(Char,Integer)]
fillOccurrence [] _ = []
fillOccurrence ((c1,amount):occurrences) c2
  | c1 == c2  =  (c1,(amount+1)):occurrences
  | otherwise = (c1,amount):(fillOccurrence occurrences c2) 

fillOccurrences :: String -> [(Char,Integer)] -> [(Char,Integer)]
fillOccurrences [] occurrences = occurrences
fillOccurrences (c:s) occurrences = fillOccurrences (s) (fillOccurrence occurrences c) 
  
initializePolymer :: String -> ([(Char,Char,Char)],[(Char,Char,Integer)],[(Char,Integer)])
initializePolymer s = (rules,(fillBuckets begin emptyBuckets),(fillOccurrences begin emptyOccurrences)) 
  where 
    (begin, rules) = getPolymerInput s
    emptyBuckets   = getEmptyBuckets rules  
    emptyOccurrences = getEmptyOccurrences s []

findMaxOccurrence :: [(Char,Integer)] -> (Char,Integer) -> (Char,Integer)
findMaxOccurrence [] (occurrence,amount) = (occurrence,amount)
findMaxOccurrence ((oc,am):remaining) (occurrence,amount)
  | am > amount = findMaxOccurrence remaining (oc,am)
  | otherwise   = findMaxOccurrence remaining (occurrence, amount)
  
findMinOccurrence :: [(Char,Integer)] -> (Char,Integer) -> (Char,Integer)
findMinOccurrence [] (occurrence,amount) = (occurrence,amount)
findMinOccurrence ((oc,am):remaining) (occurrence,amount)
  | am < amount = findMinOccurrence remaining (oc,am)
  | otherwise   = findMinOccurrence remaining (occurrence, amount)  
  
countDifference :: [(Char,Integer)] -> Integer
countDifference occurrences = amountMost - amountLst
  where
    (most, amountMost) = findMaxOccurrence occurrences (' ',0)
    (lst, amountLst)   = findMinOccurrence occurrences (' ',amountMost)

updateOccurrence :: [(Char,Integer)] -> Char -> Integer -> [(Char,Integer)]
updateOccurrence [] _ _ = []
updateOccurrence ((c1,curAmount):occurrences) c2 amount
  | c1 == c2  =  (c1,(curAmount + amount)):occurrences
  | otherwise = (c1,curAmount):(updateOccurrence occurrences c2 amount)

updateBucket :: [(Char,Char,Integer)] -> (Char,Char,Integer) -> [(Char,Char,Integer)]
updateBucket [] _ = []
updateBucket ((c1Bucket,c2Bucket,curAmount):buckets) (c1,c2, newAmount)
  | (c1Bucket == c1) && (c2Bucket == c2)  = (c1Bucket, c2Bucket,(curAmount + newAmount)):buckets
  | otherwise = (c1Bucket,c2Bucket,curAmount):(updateBucket buckets (c1,c2, newAmount))  
  
updateBuckets :: [(Char,Char,Integer)] -> [(Char,Char,Integer)] -> [(Char,Char,Integer)]
updateBuckets [] buckets = buckets
updateBuckets ((c1,c2,newAmount):remaining) buckets = updateBuckets remaining newBuckets
  where newBuckets = updateBucket buckets (c1,c2,newAmount)

updatePolyHelper :: [(Char,Char,Char)] -> (Char,Char,Integer) -> [(Char,Char,Integer)] -> [(Char,Integer)] -> ([(Char,Char,Integer)],[(Char,Integer)])
updatePolyHelper [] _  buckets occurrences = (buckets, occurrences)
updatePolyHelper ((c1,c2,inserting):rules) (c1Bucket, c2Bucket, amount) buckets occurrences
  | (c1 == c1Bucket) && (c2 == c2Bucket) = (newBuckets, newOccurrences)
  | otherwise                            = updatePolyHelper rules (c1Bucket, c2Bucket, amount) buckets occurrences
  where
    newBuckets     = updateBuckets [(c1,inserting,amount),(inserting,c2,amount),(c1,c2,(-amount))] buckets
    newOccurrences = updateOccurrence occurrences inserting amount 

updatePoly :: [(Char,Char,Integer)] -> ([(Char,Char,Char)], [(Char,Char,Integer)],[(Char,Integer)]) -> ([(Char,Char,Char)], [(Char,Char,Integer)],[(Char,Integer)])
updatePoly [] (rules, buckets, occurrences) = (rules, buckets, occurrences)
updatePoly ((c1,c2,amount):buckets) (rules, curBuckets, curOccurrences) = updatePoly buckets (rules, newBuckets, newOccurrences) 
  where 
    (newBuckets, newOccurrences) = updatePolyHelper rules (c1,c2,amount) curBuckets curOccurrences        

insertPoly40 :: Integer -> ([(Char,Char,Char)],[(Char,Char,Integer)],[(Char,Integer)]) -> Integer
insertPoly40 0 (rules,buckets,occurrences) = countDifference occurrences
insertPoly40 n (rules,buckets,occurrences) = insertPoly40 (n-1) (updatePoly buckets (rules, buckets, occurrences))

main :: IO Integer
main = do
  myFile <- openFile "input14.txt" ReadMode
  content <- hGetContents myFile
  return (insertPoly40 40 (initializePolymer content))   
