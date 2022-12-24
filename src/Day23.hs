module Day23(day23) where

import Utils
import Data.Set (Set, notMember, intersection)
import qualified Data.Set as S


parse :: [String] -> [Coord]
parse css= catMaybes $ concatMap (\(y, cs) -> (\(x,c) -> (if c=='#' then Just (x,y) else Nothing)) <$> zip [0..] cs) $ zip [0..] css


lookN, lookS, lookW, lookE :: Set Coord
lookN = S.fromList [(-1,-1), (0,-1), (1,-1)]
lookS = S.fromList [(-1,1), (0,1), (1,1)]
lookW = S.fromList [(-1,1), (-1,0), (-1,-1)]
lookE = S.fromList [(1,-1), (1,0), (1,1)]


extract :: Set Coord -> Coord
extract cs
  | cs == lookN = (0,-1)
  | cs == lookS = (0,1)
  | cs == lookW = (-1,0)
  | cs == lookE = (1,0)
  | otherwise = error "!!!"


next, next2, next3 :: Set Coord -> Set Coord
next cs
  | cs == lookN = lookS
  | cs == lookS = lookW
  | cs == lookW = lookE
  | cs == lookE = lookN
  | otherwise = error "Error in next"
next2 = next . next
next3 = next . next . next


inSpace :: Coord -> Set Coord -> Bool
inSpace e es = and $ (`notMember` es) <$> ns
  where
    ns = neighbours8 e


allEmpty :: Set Coord -> Set Coord -> Bool
allEmpty test elves = null $ test `intersection` elves


-- Really slow here - lots of duplication
propose :: Set Coord -> Set Coord -> Set (Coord, Coord)
propose dir elves = S.map go elves 
  where
    go e
      | inSpace e elves = (e,e)
      | allEmpty (S.map (e+)         dir) elves = (e, e + extract dir)
      | allEmpty (S.map (e+) $ next  dir) elves = (e, e + extract (next dir))
      | allEmpty (S.map (e+) $ next2 dir) elves = (e, e + extract (next2 dir))
      | allEmpty (S.map (e+) $ next3 dir) elves = (e, e + extract (next3 dir))
      | otherwise = (e, e)
      where
        ns = S.fromList (neighbours8 e) `S.intersection` elves


move :: Set Coord -> Set Coord -> Set Coord
move dir elves = choose proposed
  where
    proposed = propose dir elves


choose :: (Ord a) => Set (a,a) -> Set a
choose xs = S.map (\((x,y),n) -> if n == 1 then y else x) $ S.map go xs
  where
    go xx@(_, x) = (xx, S.size $ S.filter ((==x) . snd) xs)


keepMoving :: Int -> Set Coord -> Set Coord -> Set Coord
keepMoving n dir es 
  | n==0 = es
  | nxt == es = es
  | otherwise = keepMoving (n-1) (next dir) nxt
  where
    nxt = move dir es


keepMoving2 :: Int -> Set Coord -> Set Coord -> Int
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

  putStrLn $ "Day23: part1: " ++ show (score $ keepMoving 10 lookN g)
  timeIt $ putStrLn $ "Day23: part1: " ++ show (keepMoving2 0 lookN g) 

  return ()
