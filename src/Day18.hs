module Day18(day18) where

import qualified Data.Set as S
import Data.Set (Set)
import Utils ( getLines, splitOn, Coord3, manhattan3, neighbours6, floodFill )


parse :: String -> Coord3
parse s = (read $ ws!!0, read $ ws!!1, read $ ws!!2)
  where
    ws = splitOn ',' s


countSides1 :: [Coord3] -> Int
countSides1 cubes = go (6 * length cubes) cubes
  where
    go n [] = n
    go n (c:cs) = go (n - sub) cs
      where
        sub = sum $ (\d -> if manhattan3 c d == 1 then 2 else 0) <$> cs


countSides2 :: [Coord3] -> [Coord3] -> Int
countSides2 cubes airCoords = sum $ go <$> cubes
  where
    go c = length $ filter (\n -> n `notElem` cubes && n `elem` airCoords) $ neighbours6 c


air :: Set Coord3 -> [Coord3]
air droplet = floodFill (minx-1, miny-1, minz-1) getNext
  where
    ((minx, miny, minz), (maxx, maxy, maxz)) = bounds droplet
    inBounds :: Coord3 -> Bool
    inBounds (x,y,z) = (minx-1) <= x && x <= (maxx+1) && (miny-1) <= y && y <= (maxy+1) && (minz-1) <= z && z <= (maxz+1)
    
    getNext :: Coord3 -> [Coord3]
    getNext c = filter (\n -> inBounds n && n `S.notMember` droplet) $ neighbours6 c


bounds :: Set Coord3 -> (Coord3, Coord3)
bounds droplet = ( (minx, miny, minz), (maxx, maxy, maxz))
  where
    minx = minimum $ S.map (\(x,_,_) -> x)droplet
    miny = minimum $ S.map (\(_,x,_) -> x) droplet
    minz = minimum $ S.map (\(_,_,x) -> x) droplet
    maxx = maximum $ S.map (\(x,_,_) -> x) droplet
    maxy = maximum $ S.map (\(_,x,_) -> x) droplet
    maxz = maximum $ S.map (\(_,_,x) -> x) droplet


day18 :: IO ()
day18 = do
  ss <- getLines 18
  let droplet = parse <$> ss
      aircells = air $ S.fromList droplet

  putStrLn $ "Day18: part1: " ++ show (countSides1 droplet)
  putStrLn $ "Day18: part1: " ++ show (countSides2 droplet aircells)

  return ()

      