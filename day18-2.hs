import Control.Applicative ((<|>))
import Control.Monad
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

main :: IO Int
main = do
  myFile <- openFile "input18.txt" ReadMode
  content <- hGetContents myFile
  return ( part2 (parse content))
part2 xs = maximum [ magnitude (add x1 x2) | x1 <- xs, x2 <- xs, x1 /= x2]

data Snail = Value Int | Pair Snail Snail
  deriving (Show, Eq)

parse = map parseSnail . lines

parseSnail = fst . head . filter (null . snd) . readP_to_S snailP 

snailP :: ReadP Snail
snailP = valueP <|> pairP

valueP :: ReadP Snail
valueP = Value <$> (read <$> munch1 isDigit)

pairP :: ReadP Snail
pairP = do
    char '[' 
    a <- snailP 
    char ',' 
    b <- snailP 
    char ']'
    return $ Pair a b


add :: Snail -> Snail -> Snail
add a b = reduce $ Pair a b

addLeft :: Int -> Snail -> Snail
addLeft a (Value b) = Value (a+b)
addLeft a (Pair b c) = Pair (addLeft a b) c

addRight :: Snail -> Int -> Snail
addRight (Value b) a = Value (a+b)
addRight (Pair b c) a = Pair b (addRight c a)

reduce :: Snail -> Snail
reduce snail = 
  case explode 0 snail of
    (Add (l,r), _) -> undefined
    (CarryLeft _, s) -> reduce s
    (CarryRight _, s) -> reduce s
    (Exploded, s) -> reduce s
    (NoExplosion, _) -> 
        case split snail of
          Split s -> reduce s
          NoSplit -> snail

data Explosion = NoExplosion | Add (Int,Int) | CarryLeft Int | CarryRight Int | Exploded 

explode :: Int -> Snail -> (Explosion, Snail)
explode _ (Value v) = (NoExplosion, Value v)
explode 4 (Pair (Value a) (Value b)) = (Add (a,b), Value 0)
explode 4 (Pair a b) = undefined
explode depth (Pair a b) =
    case explode (depth + 1) a of
      (Add (l,r), s) -> (CarryLeft l, Pair s (addLeft r b))
      (CarryLeft l, s) -> (CarryLeft l, Pair s b)
      (CarryRight r, s) -> (Exploded, Pair s (addLeft r b))
      (Exploded, s) -> (Exploded, (Pair s b))
      (NoExplosion, _) -> 
          case explode (depth + 1) b of
            (Add (l,r), s) -> (CarryRight r, Pair (addRight a l) s)
            (CarryLeft l, s) -> (Exploded, Pair (addRight a l) s)
            (CarryRight r, s) -> (CarryRight r, Pair a s)
            (Exploded, s) -> (Exploded, (Pair a s))
            (NoExplosion, _) -> (NoExplosion, Pair a b)

data Split = NoSplit | Split Snail

split :: Snail -> Split
split (Value p) 
    | p >= 10 = Split $ Pair (Value $ (div p 2)) ( Value $ (div (p+1) 2))
    | otherwise = NoSplit
split (Pair a b)
    case split a of
      Split s -> Split (Pair s b)
      NoSplit -> 
        case split b of
          Split s -> Split (Pair a s)
          NoSplit -> NoSplit 

magnitude :: Snail -> Int
magnitude (Value i) = i
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

showSnail (Value x) = show x
showSnail (Pair a b) = "[" ++ showSnail a ++ "," ++ showSnail b ++ "]"
