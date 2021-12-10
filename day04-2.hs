import System.IO  
import Control.Monad
import Data.List.Split
import Data.List

getNumbers :: String -> [Int]
getNumbers s = map read (splitOn "," s)

readNumber :: String -> (Int,Bool)
readNumber s = (read s, False)

addRow :: String -> [[(Int,Bool)]] -> [[(Int,Bool)]]
addRow s l = l ++ [row]
  where row = map readNumber (words s)

getCard :: [String] -> [[(Int, Bool)]] -> [[(Int,Bool)]]
getCard [] l = l
getCard (x:xs) l = getCard xs newl 
  where newl = addRow x l

getCards :: [String] -> [[[(Int,Bool)]]] -> [[[(Int,Bool)]]]
getCards [] cards = cards
getCards ("":r1:r2:r3:r4:r5:xs) cards = getCards xs (cards ++ [card]) 
  where card = getCard (r1:r2:r3:r4:r5:[]) []

getContentHelper :: [String] -> ([Int], [[[(Int,Bool)]]])
getContentHelper s = ((getNumbers (head s) ) , (getCards (tail s) [] ))

getContent :: String -> ([Int], [[[(Int,Bool)]]])
getContent s = getContentHelper (lines s)

markRowCard :: Int -> [(Int,Bool)] -> [(Int, Bool)]
markRowCard _ [] = []
markRowCard n ((x,b):xs) 
  | n == x = (x,True):(markRowCard n xs)
  | otherwise = (x,b):(markRowCard n xs)

markNumberCard :: Int -> [[(Int, Bool)]] -> [[(Int, Bool)]]
markNumberCard _ [] = []
markNumberCard n (row:rows) = (markRowCard n row):(markNumberCard n rows)

markNumberCards :: Int -> [[[(Int,Bool)]]] -> [[[(Int,Bool)]]]
markNumberCards _ [] = []
markNumberCards n (card:cards) = (markNumberCard n card):(markNumberCards n cards)

isWinningRow :: [(Int, Bool)] -> Bool
isWinningRow [] = True
isWinningRow ((x,b):xs)
  | b == True = isWinningRow xs
  | otherwise = False

hasWinningRow :: [[(Int,Bool)]] -> Bool
hasWinningRow [] = False
hasWinningRow (row:rows) 
  | isWinningRow row = True
  | otherwise        = hasWinningRow rows

isWinningCard :: [[(Int,Bool)]] -> Bool
isWinningCard card = (hasWinningRow card) || (hasWinningRow (transpose card))

getWinningCard :: [[[(Int,Bool)]]] -> [[(Int,Bool)]]
getWinningCard [] = []
getWinningCard (card:cards) 
  | isWinningCard card = card
  | otherwise          = getWinningCard cards
  
filterWinning :: [[[(Int,Bool)]]] -> [[[(Int,Bool)]]]
filterWinning [] = []
filterWinning (card:cards)
  | isWinningCard card = filterWinning cards
  | otherwise          = card:(filterWinning cards)

getLastWinningOutCome ::  [Int] -> [[[(Int,Bool)]]] -> (Int, [[(Int, Bool)]])
getLastWinningOutCome (n:numbers) cards
  | newCards == [] = (n, (markNumberCard n (head cards)))
  | otherwise      = getLastWinningOutCome numbers newCards
  where 
    newCards = filterWinning (markNumberCards n cards)

getLastWinning :: ([Int], [[[(Int,Bool)]]]) -> (Int, [[(Int, Bool)]])
getLastWinning (numbers, cards) = getLastWinningOutCome numbers cards 

getSumOfUnmarkedRow :: [(Int,Bool)] -> Int
getSumOfUnmarkedRow [] = 0
getSumOfUnmarkedRow ((n,b):numbers) 
  | b == False = n + (getSumOfUnmarkedRow numbers)
  | otherwise  = getSumOfUnmarkedRow numbers

getSumOfUnmarkedCard :: [[(Int,Bool)]] -> Int
getSumOfUnmarkedCard [] = 0
getSumOfUnmarkedCard (row:rows) = (getSumOfUnmarkedRow row) + (getSumOfUnmarkedCard rows)

getFinalScore :: (Int, [[(Int, Bool)]]) -> Int
getFinalScore (n, card) = n * (getSumOfUnmarkedCard card)

main :: IO Int
main = do
  myFile <- openFile "input04.txt" ReadMode
  content <- hGetContents myFile
  return (getFinalScore (getLastWinning (getContent content)))
