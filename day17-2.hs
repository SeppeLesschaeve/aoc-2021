import System.IO  
import Control.Monad
import Data.List.Split

updateDX :: Int -> Int 
updateDX dx
  | dx > 0 = dx - 1
  | dx < 0 = dx + 1
  | otherwise  = dx

step :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
step (x,y) (dx, dy) = (x, y) : step (x + dx, y + dy) ((updateDX dx), dy - 1) 

isTarget :: Int -> Int -> (Int, Int) -> (Int, Int) -> Bool
isTarget x y (xmin, xmax) (ymin, ymax) = xmin <= x && x <= xmax && ymin <= y && y <= ymax
 
getTrajectory ymin = takeWhile (\(x, y) -> y >= ymin) . step (0, 0)
isValid trajectory (xmin, xmax) (ymin, ymax) = any (\(x, y) -> isTarget x y (xmin, xmax) (ymin, ymax)) trajectory

allTrajectories (xmin, xmax) (ymin, ymax) =
  [ trajectory | dx <- [1 .. xmax] , dy <- [ymin .. negate ymin] , let trajectory = getTrajectory ymin (dx, dy), isValid trajectory (xmin, xmax) (ymin, ymax)]

amountVelocitiesPossible :: ((Int, Int),(Int, Int)) -> Int
amountVelocitiesPossible ((xmin, xmax), (ymin, ymax)) = length (allTrajectories (xmin, xmax) (ymin, ymax) )

getBounds :: String -> (Int, Int)
getBounds s = (read (boundsStrings!!0), read (boundsStrings!!1))
  where 
    bounds = tail ( tail ( s ) )
    boundsStrings = splitOn ".." bounds 

getTargetArea :: String -> ((Int,Int),(Int, Int))
getTargetArea s = (xBounds, yBounds)
  where 
    spaceWords = words s
    xBounds = getBounds (head (splitOn "," (spaceWords!!2)))
    yBounds = getBounds (spaceWords!!3)
  
main :: IO Int
main = do
  myFile <- openFile "input17.txt" ReadMode
  content <- hGetContents myFile
  return (amountVelocitiesPossible (getTargetArea content))
