module Day14(day14) where

import Data.HashSet  (HashSet, union, insert, member, fromList, unions, empty, toList)
import Utils ( dn, getLines, lt, rt, splitOn, Coord )
import Data.List ( find )


parseCoord :: String -> Coord
parseCoord s = (read $ ws!!0, read $ ws!!1)
  where
    ws = splitOn ',' s


-- The coord are alternate words...
parseLine :: String -> [Coord]
parseLine s = parseCoord <$> ws
  where
    ws :: [String]
    ws = snd <$> filter (odd . fst) (zip [(1::Int)..] (words s))


toSet :: [Coord] -> HashSet Coord
toSet = unions . go []
  where
    go :: [HashSet Coord] -> [Coord] -> [HashSet Coord]
    go _ [] = error "Shouldn't get here in toRange"
    go rs [_] = rs
    go rs ((x1,y1):e@(x2,y2):es)
      | x1==x2 = go (fromList ((x1,) <$> [(min y1 y2)..(max y1 y2)]) : rs) (e:es)
      | y1==y2 = go (fromList ((,y1) <$> [(min x1 x2)..(max x1 x2)]) : rs) (e:es)
      | otherwise = error "There isn't a repeated coordinate"


sand :: Coord
sand = (500, 0)


grain :: Int -> HashSet Coord -> Coord
grain maxrow rocks = go sand
  where
    go p@(_, py)
      | py==maxrow = p
      | Just next <- find (\x -> not (x `member` rocks)) [p+dn, p+dn+lt, p+dn+rt] = go next 
      | otherwise = p


grains :: Bool -> Int -> HashSet Coord -> Int
grains is1 maxrow = go 0
  where
    go :: Int -> HashSet Coord -> Int
    go n rocks
      | py == maxrow = if is1 then n else go (n+1) (p `insert` rocks)
      | p == sand = n+1
      | otherwise = go (n+1) (p `insert` rocks)
      where
        p@(_, py) = grain maxrow rocks
          

day14 :: IO ()
day14 = do
  ss <- getLines 14
  let g = parseLine <$> ss
      rs :: HashSet Coord
      rs = unions $ toSet <$> g
      limit = maximum (snd <$> toList rs) + 1

  putStrLn $ "Day14: part1: " ++ show (grains True limit rs )
  putStrLn $ "Day14: part2: " ++ show (grains False limit rs )
  
  return ()
