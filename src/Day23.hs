module Day23(day23) where

import Utils
import Data.Set (Set, notMember, intersection)
import qualified Data.Set as S

data Dir = N | S | W | E deriving (Eq, Show)

next, next2, next3 :: Dir -> Dir
next N = S
next S = W
next W = E
next E = N
next2 = next . next
next3 = next . next . next

toCoord :: Dir -> Coord
toCoord N = up
toCoord S = dn
toCoord E = rt
toCoord W = lt


parse :: [String] -> [Coord]
parse css= catMaybes $ concatMap (\(y, cs) -> (\(x,c) -> (if c=='#' then Just (x,y) else Nothing)) <$> zip [0..] cs) $ zip [0..] css


inSpace :: Coord -> Set Coord -> Bool
inSpace e es = and $ (`notMember` es) <$> ns
  where
    ns = neighbours8 e


-- Really slow here - lots of duplication
propose :: Dir -> Set Coord -> Set (Coord, Coord)
propose dir elves = S.map go elves 
  where
    go e
      | inSpace e elves = (e,e)
      | and (get         dir) = (e, e + toCoord       dir)
      | and (get $ next  dir) = (e, e + toCoord (next dir))
      | and (get $ next2 dir) = (e, e + toCoord (next2 dir))
      | and (get $ next3 dir) = (e, e + toCoord (next3 dir))
      | otherwise = (e, e)
      where
        ns@(n:_) = (`notMember` elves) <$> neighbours8 e
        get :: Dir -> Set Bool
        get N = S.fromList $ take 3 ns
        get E = S.fromList $ take 3 $ drop 2 ns
        get S = S.fromList $ take 3 $ drop 4 ns
        get W = S.fromList $ take 3 $ drop 6 ns ++ [n]


move :: Dir -> Set Coord -> Set Coord
move dir elves = choose proposed
  where
    proposed = propose dir elves


choose :: (Ord a) => Set (a,a) -> Set a
choose xs = S.map (\((x,y),n) -> if n == 1 then y else x) $ S.map go xs
  where
    go xx@(_, x) = (xx, S.size $ S.filter ((==x) . snd) xs)


keepMoving :: Int -> Dir -> Set Coord -> Set Coord
keepMoving n dir es 
  | n==0 = es
  | nxt == es = es
  | otherwise = keepMoving (n-1) (next dir) nxt
  where
    nxt = move dir es


keepMoving2 :: Int -> Dir -> Set Coord -> Int
keepMoving2 n dir es
  | nxt == es = n+1
  | otherwise = keepMoving2 (n+1) (next dir) nxt
  where
    nxt = move dir es


score :: Set Coord -> Int
score elves = (maxx-minx+1)*(maxy-miny+1) - length elves
  where
    minx = minimum $ S.map fst elves
    maxx = maximum $ S.map fst elves
    miny = minimum $ S.map snd elves
    maxy = maximum $ S.map snd elves


day23 :: IO ()
day23 = do
  ss <- getLines 23
  let g = S.fromList $ parse ss

  putStrLn $ "Day23: part1: " ++ show (score $ keepMoving 10 N g)  --3923
  timeIt $ putStrLn $ "Day23: part1: " ++ show (keepMoving2 0 N g) --1019

  return ()
