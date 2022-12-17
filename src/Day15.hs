module Day15(day15) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Data.Sequence (Seq(..), ViewR(..), empty, singleton, viewr)
import Utils ( sort, getLines, manhattan, Coord )


parse :: String -> (Coord, Coord)
parse s = ((x1, y1), (x2, y2))
  where
    ws = words s
    x1 = read $ drop 2 $ take (length (ws!!2) - 1) $ ws!!2
    y1 = read $ drop 2 $ take (length (ws!!3) - 1) $ ws!!3
    x2 = read $ drop 2 $ take (length (ws!!8) - 1) $ ws!!8
    y2 = read $ drop 2 $ ws!!9


beaconDistances :: [(Coord, Coord)] -> Map Coord Int
beaconDistances = foldl (\mp (s@(sx, sy), b) -> M.insert (sx, sy) (manhattan s b) mp) M.empty


-- Disallowed intervals on the chosen (yval) row (ie. points nearer than the beacon)
intervals :: Int -> Map Coord Int -> [(Int, Int)]
intervals yval mp = concat $ M.elems $ M.mapWithKey go mp
  where
    go (sx, sy) d = [(sx - (d - ydist), sx + (d - ydist)) | d>=ydist]
      where
        ydist = abs (yval - sy)


-- These are the lines which make the diamond
diamond :: Coord -> Int -> [(Int, Int)]
diamond (cx, cy) d =  [(slope, cy-slope*cx+offset*(d+1)) | slope <- [-1,1], offset <- [-1,1]]


findCoord :: Map Coord Int -> Int
findCoord mp = 4000000*cx+cy
  where
    -- The outside coord must be one of the interceptions
    (cx, cy) = head $ filter filterWith intercepts
    (negLines, posLines) = splitAt (2 * M.size mp) $ sort $ concatMap snd  $ M.toList $ M.mapWithKey diamond mp
    -- Intercepts of all the negatively sloped lines with all the positively sloped lines
    intercepts = sort [((d-c) `div` 2,(d+c) `div` 2) | (_,d) <- negLines, (_, c) <- posLines]

    -- In bounds and further away than the identified beacon
    filterWith i@(ix,iy) = ix>=0 && ix<=4000000 && iy>=0 && iy<=4000000 && and ((\s -> manhattan s i > (mp ! s)) <$> M.keys mp)


-- Use Seq so that we have viewr and :>
addIntervals :: [(Int, Int)] -> Seq (Int, Int)
addIntervals is = go empty $ sort is
  where
    go :: Seq (Int, Int) -> [(Int, Int)] -> Seq (Int, Int)
    go acc [] = acc
    go ins ((nlo, nhi):others) = case viewr ins of
        EmptyR -> go (singleton (nlo, nhi)) others
        acc :> (llo, lhi) -> if nlo <= lhi+1  then
                               go (acc :|> (llo, nhi)) others
                               else
                               go (ins :|> (nlo, nhi)) others


day15 :: IO ()
day15 = do
  ss <- getLines 15
  let g = parse <$> ss
      distanceMap = beaconDistances g
      part1row = 2000000

  putStrLn $ "Day15: part1: " ++ show (sum $ uncurry (flip (-)) <$> addIntervals (intervals part1row distanceMap))
  putStrLn $ "Day15: part2: " ++ show (findCoord distanceMap)

  return ()

