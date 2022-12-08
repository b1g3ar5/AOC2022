module Day8(day8) where

import qualified Data.Map.Strict as M
import Utils ( Coord, getLines, lt, rt, up, dn )
import Data.List ( nub, transpose )


parse :: [String] -> [[(Coord, Int)]]
parse css = (\(y, cs) -> (\(x, c) -> ((x,y),read [c])) <$> zip [0..] cs) <$> zip [0..] css


countTrees :: [(Coord, Int)] -> [Coord]
countTrees [] = error "There aren't any trees"
countTrees (x:xs) = go [fst x] (snd x) xs
  where
    go :: [Coord] -> Int -> [(Coord, Int)] -> [Coord]
    go _ _ [] = error "Shouldn't be here"
    go cs mx [(c,t)]
      | t>mx = c:cs
      | otherwise = cs
    go cs mx ((c,t):ts)
      | t>mx = go (c:cs) t ts
      | otherwise = go cs mx ts


countAll :: [[(Coord, Int)]] -> Int
countAll xss = length $ nub $ a++b++c++d
  where
    a = concatMap countTrees xss
    b = concatMap countTrees $ reverse <$> xss
    c = concatMap countTrees $ transpose xss
    d = concatMap countTrees $ reverse <$> transpose xss


scenicScore :: Coord -> M.Map Coord Int -> Int
scenicScore t mp = go 0 0 up t * go 0 0 dn t * go 0 0 lt t * go 0 0 rt t
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


maxCoord :: Int
maxCoord = 98
inBounds :: Coord -> Bool
inBounds (x,y) = (x<=maxCoord) && (y<=maxCoord) && (x>=0) && (y>=0)

day8 :: IO ()
day8 = do
  ss <- getLines 8
  let g = parse ss
      mp = M.fromList $ concat g

  putStrLn $ "Day8: part1: " ++ show (countAll g)
  putStrLn $ "Day8: part1: " ++ show (maximum $ M.mapWithKey (\k _ -> scenicScore k mp) mp)

  return ()
