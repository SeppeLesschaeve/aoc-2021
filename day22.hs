import Data.Bifunctor (bimap)
import Data.Maybe (catMaybes,isNothing)
import System.IO

main :: IO String
main = do
  myFile <- openFile "input22.txt" ReadMode
  content <- hGetContents myFile
  return ((uncurry f . bimap doa dob . join (,) . parse) content)
    where f a b = unlines [show a ++ "\t" ++ show b]

region = (r,r,r) where r = (-50,50)
doa = sum . map volume . go [] . catMaybes . map f
    where f (b,x) = (,) b <$> overlap region x
dob = sum . map volume . go []

type Range = (Int,Int)
lo = fst
hi = snd
type Cuboid = (Range, Range, Range)
xr (x,_,_) = x
yr (_,y,_) = y
zr (_,_,z) = z

join :: (Monad m) => m (m b) -> m b
join = (>>= id)

roverlap :: Range -> Range -> Maybe Range
roverlap a b
    | fst c <= snd c = Just c
    | otherwise      = Nothing
    where c = (max (lo a) (lo b), min (hi a) (hi b))

rsplit :: Range -> Range -> (Maybe Range, Maybe Range, Maybe Range)
rsplit a b = ( join $ roverlap a <$> f (lo a) (lo b - 1)
             , roverlap a b
             , join $ roverlap a <$> f (hi b + 1) (hi a) )
    where f x y = if y < x then Nothing else Just (x,y)

overlap :: Cuboid -> Cuboid -> Maybe Cuboid
overlap a b = (,,) <$> r xr <*> r yr <*> r zr
    where r f = roverlap (f a) (f b)

volume :: Cuboid -> Integer
volume a = size (xr a) * size (yr a) * size (zr a)
    where size r = fromIntegral (hi r - lo r + 1)

parse = map parseLine . lines

parseLine :: String -> (Bool, Cuboid)
parseLine = f . words . map (\c -> if elem c "=,." then ' ' else c)
    where f (b:_:xm:xx:_:ym:yx:_:zm:zx:_)
              = ( b == "on"
                , ( (read xm, read xx)
                  , (read ym, read yx)
                  , (read zm, read zx)))
          f _ = error "unreadable line"

go :: [Cuboid] -> [(Bool,Cuboid)] -> [Cuboid]
go p [] = p
go p ((b,x):xs) = flip go xs
                  . (if b then (x:) else id) $ concatMap (csplit x) p
csplit :: Cuboid -> Cuboid -> [Cuboid]
csplit b a
    | isNothing (overlap a b) = pure a
    | otherwise
        = catMaybes
          $ ((,,) <$> xl <*> pure (yr a) <*> pure (zr a))
          : ((,,) <$> xc <*> yl <*> pure (zr a))
          : ((,,) <$> xc <*> yc <*> zl)
          : ((,,) <$> xc <*> yc <*> zh)
          : ((,,) <$> xc <*> yh <*> pure (zr a))
          : ((,,) <$> xh <*> pure (yr a) <*> pure (zr a))
          : []
    where (xl,xc,xh) = rsplit (xr a) (xr b)
          (yl,yc,yh) = rsplit (yr a) (yr b)
          (zl,zc,zh) = rsplit (zr a) (zr b)
