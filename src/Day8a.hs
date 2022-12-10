module Day8a(day8a) where

import qualified Data.Array.IArray as A
import Utils ( Coord, getLines, lt, rt, up, dn )
import Data.List ( nub, groupBy, sort )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE


parse :: [String] -> A.Array Coord Int
parse css = A.array ((0,0), (maxCoord-1, maxCoord-1)) $ concatMap (\(y, cs) -> (\(x, c) -> ((x,y),read [c])) <$> zip [0..] cs) $ zip [0..] css


maxCoord :: Int
maxCoord = 99
inBounds :: Coord -> Bool
inBounds (x,y) = (x<maxCoord) && (y<maxCoord) && (x>=0) && (y>=0)


countLine :: NonEmpty (Coord, Int) -> [Coord]
countLine (x:|xs) = go [fst x] (snd x) $ NE.fromList xs
  where
    go :: [Coord] -> Int -> NonEmpty (Coord, Int) -> [Coord]
    go cs mx ((c,t):| [])
      | t>mx = c:cs
      | otherwise = cs
    go cs mx ((c,t):|ts)
      | t>mx = go (c:cs) t $ NE.fromList ts
      | otherwise = go cs mx $ NE.fromList ts


countAll :: A.Array Coord Int -> Int
countAll arr = length $ nub $ concatMap (\f -> countFn $ f nexss) directionFns
  where
    xss :: [[(Coord, Int)]]
    xss = groupBy (\((i,_),_) ((j,_),_) -> i==j) $ sort $ A.assocs arr
    countFn = concatMap countLine
    nexss = NE.fromList $ NE.fromList <$> xss
    directionFns = [ id
                 , (NE.reverse <$>)
                 , NE.transpose
                 , (NE.reverse <$>) . NE.transpose]


scenicDistance :: A.Array Coord Int -> Coord -> Int
scenicDistance mp t = product $ go 0 0 t <$> [up, dn, rt, lt]
  where
    treeHgt = mp A.! t
    go distance hgt lst move
      | not (inBounds nxt) = distance
      | nxtHgt >= treeHgt = distance + if nxtHgt >= hgt then 1 else 0
      | nxtHgt >= hgt = go (distance + 1) nxtHgt nxt move
      | otherwise = go (distance + 1) hgt nxt move
      where
        nxt = lst + move
        nxtHgt = mp A.! nxt


day8a :: IO ()
day8a = do
  ss <- getLines 8
  --let ss = test
  let arr = parse ss

  putStrLn $ "Day8a: part1: " ++ show (countAll arr)
  putStrLn $ "Day8a: part1: " ++ show (maximum $ scenicDistance arr <$> A.indices arr)

  return ()
