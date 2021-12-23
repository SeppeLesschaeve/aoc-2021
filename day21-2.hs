{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State.Strict
import Data.Array ((!), array, range)
import Data.List (transpose)

main = print $ part2 1 2

part2 a b = dp ! (0,a,b,0,0)
  where
    dp = array bounds [ (p, wins p) | p <- range bounds ]
    wins (t,p1,p2,s1,s2)
      | s1 >= 21 = [1,0]
      | s2 >= 21 = [0,1]
      | t == 0 = map sum $ transpose
                [dp ! (1,p ,p2,s1+p,s2) | p <- wrap . (p1 +) <$> possibleRolls]
      | t == 1 = map sum $ transpose
                [dp ! (0,p1,p ,s1,s2+p) | p <- wrap . (p2 +) <$> possibleRolls]
    possibleRolls = sum3 <$> [1,2,3] <*> [1,2,3] <*> [1,2,3]

bounds :: ((Int,Int,Int,Int,Int),(Int,Int,Int,Int,Int))
bounds = ((0,1,1,0,0), (1,10,10,30,30))
    
sum3 :: Int -> Int -> Int -> Int   
sum3 a b c = a + b + c 

wrap :: Int -> Int    
wrap p | p > 10 = wrap $ p - 10
       | otherwise = p                        
