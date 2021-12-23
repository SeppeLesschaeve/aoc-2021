solve :: (Int,Int) -> (Int,Int) -> Int -> Int
solve (a,b) (s,t) n
    | s >= 1000 = n*t-t
    | t >= 1000 = n*s-s
    | otherwise = solve (c,d) (u,v) (n+3)
    where 
      diceScore = getDiceScore n
      (c,d) = (mod (a-1+diceScore*mod n 2) 10+1, mod (b-1+diceScore*(1-mod n 2)) 10+1)
      (u,v) = (s+c*mod n 2, t+d*(1-mod n 2))

getDiceScore :: Int -> Int
getDiceScore n = sum [ mod (n+m) 100  | m <- [0..2]]   

main = print $ solve (1,2) (0,0) 1
