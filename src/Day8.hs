module Day8(day8) where

import qualified Data.Map.Strict as M
import Utils ( Coord, getLines, lt, rt, up, dn )
import Data.List ( nub )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE


parse :: [String] -> [[(Coord, Int)]]
parse css = (\(y, cs) -> (\(x, c) -> ((x,y),read [c])) <$> zip [0..] cs) <$> zip [0..] css


maxCoord :: Int
maxCoord = 99
inBounds :: Coord -> Bool
inBounds (x,y) = (x<maxCoord) && (y<maxCoord) && (x>=0) && (y>=0)


countTrees :: NonEmpty (Coord, Int) -> [Coord]
countTrees (x:|xs) = go [fst x] (snd x) $ NE.fromList xs
  where
    go :: [Coord] -> Int -> NonEmpty (Coord, Int) -> [Coord]
    go cs mx ((c,t):| [])
      | t>mx = c:cs
      | otherwise = cs
    go cs mx ((c,t):|ts)
      | t>mx = go (c:cs) t $ NE.fromList ts
      | otherwise = go cs mx $ NE.fromList ts


countAll :: [[(Coord, Int)]] -> Int
countAll xss = length $ nub $ a++b++c++d
  where
    nexss = NE.fromList $ NE.fromList <$> xss
    a = concatMap countTrees nexss
    b = concatMap countTrees $ NE.reverse <$> nexss
    c = concatMap countTrees $ NE.transpose nexss
    d = concatMap countTrees $ NE.reverse <$> NE.transpose nexss


scenicDistance :: Coord -> M.Map Coord Int -> Int
scenicDistance t mp = go 0 0 up t * go 0 0 dn t * go 0 0 lt t * go 0 0 rt t
  where
    treeHgt = mp M.! t
    go distance hgt move lst
      | not (inBounds nxt) = distance
      | nxtHgt >= treeHgt = distance + if nxtHgt>=hgt then 1 else 0
      | nxtHgt >= hgt = go (distance+1) nxtHgt move nxt
      | otherwise = go (distance+1) hgt move nxt
      where
        nxt = lst + move
        nxtHgt = mp M.! nxt



day8 :: IO ()
day8 = do
  ss <- getLines 8
  let g = parse ss
      mp = M.fromList $ concat g

  putStrLn $ "Day8: part1: " ++ show (countAll g)
  putStrLn $ "Day8: part1: " ++ show (maximum $ M.mapWithKey (\k _ -> scenicDistance k mp) mp)

  return ()
