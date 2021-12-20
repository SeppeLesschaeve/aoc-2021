{-# LANGUAGE LambdaCase #-}
import           Data.Map ( Map )
import qualified Data.Map   as M
import           Data.List.Split
import           Data.List
import           Data.Ord

main :: IO Int
main = do
  input <- readFile "input20.txt"
  return (solve (parseInput input))

parseInput :: String -> (String, Map (Int, Int) Char)
parseInput inp =
  ( head results
  , M.fromList
    [ ((r, c), point)
    | (r, row  ) <- zip [0 ..] (lines $ last results)
    , (c, point) <- zip [0 ..] row
    ]
  )
  where results = splitOn "\n\n" inp

solve :: (String, Map (Int, Int) Char) -> Int
solve parsed = length . M.toList . M.filter (== '#') $ steps 50 (snd parsed) (fst parsed) False

steps :: Int -> Map (Int, Int) Char -> String -> Bool -> Map (Int, Int) Char
steps 0 img alg onOff = img
steps n img alg onOff = steps (n - 1) nextStep alg (not onOff)
  where nextStep = step img alg onOff

step :: Map (Int, Int) Char -> String -> Bool -> Map (Int, Int) Char
step img alg onOff = M.mapWithKey (\k v -> enhance k alg bordered onOff)
                                  bordered
  where bordered = addBorder img onOff

addBorder :: Map (Int, Int) Char -> Bool -> Map (Int, Int) Char
addBorder img onOff = foldl
  (flip
    (M.alter
      (\case
        Just val -> Just val
        Nothing  -> if onOff then Just '#' else Just '.'
      )
    )
  )
  img
  indicesToCheck
  where indicesToCheck = concatMap (`neighborIndices` img) (M.keys img)

neighborIndices :: (Int, Int) -> Map (Int, Int) Char -> [(Int, Int)]
neighborIndices (r0, c0) img =
  [ (r, c) | r <- [r0 - 1, r0, r0 + 1], c <- [c0 - 1, c0, c0 + 1] ]

neighbors :: (Int, Int) -> Map (Int, Int) Char -> Bool -> String
neighbors (r0, c0) img onOff =
  map (\index -> M.findWithDefault (if onOff then '#' else '.') index img)
    $ neighborIndices (r0, c0) img

enhance :: (Int, Int) -> String -> Map (Int, Int) Char -> Bool -> Char
enhance point alg img onOff = alg !! toIndex (neighbors point img onOff)

toIndex :: String -> Int
toIndex = foldl (\acc n -> acc * 2 + n) 0 . map toBin
 where
  toBin '.' = 0
  toBin '#' = 1

render :: Map (Int, Int) Char -> String
render img =
  unlines
    $ map renderRow
    $ groupBy (\a b -> fst (fst a) == fst (fst b))
    $ sortBy (comparing fst)
    $ M.toList img

renderRow :: [((Int, Int), Char)] -> String
renderRow = map snd
