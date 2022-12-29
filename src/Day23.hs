module Day23(day23) where

import Utils
import Data.Set (Set, notMember)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M


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


propose :: Dir -> Set Coord -> Set Coord
propose dir elves = S.fromList $ M.foldMapWithKey (\k es -> if length es > 1 then es else [k]) $ S.foldl' go M.empty elves 
  where
    -- Make a map from prosed moves to elves proosing that move
    go :: Map Coord [Coord] -> Coord -> Map Coord [Coord]
    go mp e
      | inSpace e elves = M.insertWith (++) e [e] mp
      | and (get         dir) = M.insertWith (++) (e + toCoord        dir)  [e] mp 
      | and (get $ next  dir) = M.insertWith (++) (e + toCoord (next  dir)) [e] mp 
      | and (get $ next2 dir) = M.insertWith (++) (e + toCoord (next2 dir)) [e] mp 
      | and (get $ next3 dir) = M.insertWith (++) (e + toCoord (next3 dir)) [e] mp 
      | otherwise = M.insertWith (++) e [e] mp
      where
        ns@(n:_) = (`notMember` elves) <$> neighbours8 e
        get :: Dir -> Set Bool
        get N = S.fromList $ take 3 ns
        get E = S.fromList $ take 3 $ drop 2 ns
        get S = S.fromList $ take 3 $ drop 4 ns
        get W = S.fromList $ take 3 $ drop 6 ns ++ [n]


keepMoving :: Int -> Int -> Dir -> Set Coord -> (Int, Set Coord)
keepMoving inc n dir es
  | n+inc == 0 = (n+inc, nxt)
  | nxt == es = (n+inc, es)
  | otherwise = keepMoving inc (n+inc) (next dir) nxt
  where
    nxt = propose dir es


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

  putStrLn $ "Day23: part1: " ++ show (score $ snd $ keepMoving (-1) 10 N g)  --3923
  timeIt $ putStrLn $ "Day23: part2: " ++ show (fst $ keepMoving 1 0 N g) --1019

  return ()
