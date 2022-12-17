module Day17(day17) where

import qualified Data.Set as S
import Data.Set as S ( Set )
import Utils ( fromJust, getLines, lt, rt, Coord, isNothing )
import qualified Utils as U
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

type Rock = Set Coord
type Gas = String
type Rockix = Int
type Gasix = Int

-- reverse up/dn from Utils
up', dn' :: Coord
up' = U.dn
dn' = U.up

r1, r2, r3, r4, r5 :: Coord -> Rock
r1 c = S.fromList [c, c+rt, c+rt+rt, c+rt+rt+rt]
r2 c = S.fromList [c+up', c+rt, c+rt+up', c+rt+up'+up', c+rt+rt+up']
r3 c = S.fromList [c, c+rt, c+rt+rt, c+rt+rt+up', c+rt+rt+up'+up']
r4 c = S.fromList [c, c+up', c+up'+up', c+up'+up'+up']
r5 c = S.fromList [c, c+rt, c+up', c+up'+rt]


rocks :: [Coord -> Rock]
rocks = [r1, r2, r3, r4, r5]

ltEdge, rtEdge :: Rock -> Int
ltEdge rs = minimum $ S.map fst rs
rtEdge rs = maximum $ S.map fst rs


moveLt, moveRt, moveDn :: Rock -> Rock -> Rock
moveLt rs r
  | ltEdge r == 0 = r
  | S.disjoint newr rs = newr
  | otherwise = r
  where
    newr = S.map (lt +) r

moveRt rs r
  | rtEdge r == 6 = r
  | S.disjoint newr rs = newr
  | otherwise = r
  where
    newr = S.map (rt +) r

moveDn _ = S.map (dn' +) 


height :: (Rock, Int) -> Int
height (cs, n) = n + maximum (S.map snd cs)


addRock :: Gas -> (Rock, Gasix) -> Rockix -> (Rock, Gasix)
addRock gas (inRock, inGasix) rix = go (start, inGasix)
  where
    gn = length gas
    start :: Rock
    start = (rocks !! rix) (2, height (inRock, 0) + 4)
    go :: (Rock, Gasix) -> (Rock, Gasix)
    go (r, gix)
      | S.disjoint down inRock = go (down, (gix+1) `mod` gn)
      | otherwise = (S.union newr inRock, (gix+1) `mod` gn)
      where
        newr = if (gas !! gix) == '<' then moveLt inRock r else moveRt inRock r
        down = moveDn inRock newr


type Memo = HashMap (Rock, Gasix, Rockix) (Int, Int)

addRocks :: Int -> Gas -> (Rock, Int)
addRocks n gas = go H.empty n (start, 0, 0) 0
  where
    start = S.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0)]

    go :: Memo -> Int -> (Rock, Gasix, Int) -> Rockix -> (Rock, Int)
    go _ 0 (inRock, _, h)  _ = (inRock, h)
    go memo m (inRock, gix, hin) rix
      | isNothing lu = go (H.insert (newRock2, newGix, rix+1) (m, hin+hadd) memo) (m-1) (newRock2, newGix, hin+hadd) ((rix + 1) `mod` 5)
      | otherwise = go memo newm (newRock2, newGix, newh) ((rix+1) `mod` 5)
      where
        (newRock1, newGix) = addRock gas (inRock, gix) rix
        (newRock2, hadd) = prune newRock1
        -- Memoisation...
        lu = memo H.!? (newRock2, newGix, (rix+1) `mod` 5)
        (oldm, oldh) = fromJust lu
        -- Changes
        dm = oldm - m
        dh = hin + hadd - oldh
        repetitions = m `div` dm
        -- Apply the repetitions
        newm = m - repetitions * dm - 1
        newh = hin + hadd + repetitions * dh


prune :: Rock -> (Rock, Int)
prune r =  (S.map (\(x,y) -> (x,y-sub)) $ S.filter (\(_,y) -> y > sub) r, sub)
  where
    h = height (r,0)
    pruneHeight = 30
    sub = if h < pruneHeight then 0 else h - pruneHeight


day17 :: IO ()
day17 = do
  ss <- getLines 17
  let gas = head ss
      bigNumber = 1000000000000
  
  putStrLn $ "Day17: part1: " ++ show (height $ addRocks 2022 gas)
  putStrLn $ "Day17: part2: " ++ show (height $ addRocks bigNumber gas)

  return ()
