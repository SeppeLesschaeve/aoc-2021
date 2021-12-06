import System.IO  
import Control.Monad
import Data.List.Split
import Data.List

getLanternFishes :: String -> [Int]
getLanternFishes s = map read (splitOn "," s)

performEvolution :: [Int] -> [Int]
performEvolution [] = []
performEvolution (fish:fishes) 
  | fish == 0 = 6:8:(performEvolution fishes)
  | otherwise = (fish - 1):(performEvolution fishes)

calculate80Helper :: Int -> [Int] -> Int
calculate80Helper n fishes 
  | n == 80 = length fishes 
  | otherwise = calculate80Helper (n+1) newFishes
  where newFishes = performEvolution fishes

calculate80 :: [Int] -> Int
calculate80 fishes = calculate80Helper 0 fishes

main :: IO Int
main = do
  myFile <- openFile "input6.txt" ReadMode
  content <- hGetContents myFile
  return (calculate80 (getLanternFishes content))
