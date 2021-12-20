import Data.Either
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO  
import Control.Monad


main :: IO Int
main = do
  myFile <- openFile "input19.txt" ReadMode
  content <- hGetContents myFile
  return (solve ( getScanners content))
  
solve :: [[[Int]]] -> Int
solve (x:xs) = maximum . map (uncurry manhattan) . pick2 . map snd $ ( align [(x, [0, 0, 0])] [x] xs)

getScanners :: String -> [[[Int]]]
getScanners = map (map (map read . splitOn "," )  . tail) . splitOn [""] . lines

align :: [([[Int]], [Int])] -> [[[Int]]] -> [[[Int]]] -> [([[Int]], [Int])]
align result _ [] = result
align result (ref:refs) scanners = align (found ++ result) (map fst found ++ refs) notFound
  where (found, notFound) = partitionEithers
          [ maybe (Right scanner) Left . safeHead $ alignHelper ref scanner | scanner <- scanners]

alignHelper :: [[Int]] -> [[Int]] -> [([[Int]], [Int])]
alignHelper a b = [(map (zipWith (+) pos) o, pos) | o <- getOrientations b, pos <- getOverlappingScanners a o]

getOverlappingScanners :: [[Int]] -> [[Int]] -> [[Int]]
getOverlappingScanners as bs = Map.keys . Map.filter (>= 12) . Map.fromListWith (+) . map (\x -> (x, 1)) $ zipWith (-) <$> as <*> bs

getOrientations :: [[Int]] -> [[[Int]]]
getOrientations ps = transpose $ map getPosOrientations ps

getPosOrientations :: [Int] -> [[Int]]
getPosOrientations p = scanl (flip ($)) p [roll,turn,turn,turn,roll,turn,turn,turn,roll,turn,turn,turn, roll.turn.roll.roll, turn,turn,turn,roll,turn,turn,turn,roll,turn,turn,turn]
  where
    roll [x,y,z] = [x,z,-y]
    turn [x,y,z] = [-y,x,z]
    
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

pick2 :: [a] -> [(a,a)]
pick2 [] = []
pick2 (x:xs) = map (\y -> (x , y)) xs ++ pick2 xs

manhattan :: [Int] -> [Int] -> Int
manhattan a b = sum $ map abs $ zipWith (-) a b
