module Day24(day24) where

import Utils ( fromJust, isJust, getLines, neighbours4, second, Coord )
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S


data Direction = Up | Dn | Lt | Rt deriving (Eq, Show, Ord)
type Snow = Map Direction (Set Coord)
data State = State {time :: Int , pos :: Coord} deriving (Eq, Show, Ord)


-- The show repeats with period lcm of the rows and cols
-- so we don't need to revisit states with the same position and time mod this lcm
normalise :: State -> State
normalise (State t p) = State (t `mod` lcm gridRows gridCols) p


gridRows, gridCols :: Int
gridRows = 25
gridCols = 120


parseSnow :: [String] -> Snow
parseSnow css = M.fromList $ (\d -> (d, S.map fst $ S.filter ((==d) . snd) s)) <$> [Up, Dn, Lt, Rt]
  where
    s :: Set (Coord, Direction)
    s = S.fromList $ second fromJust <$> snow
    snow = filter (isJust . snd) $ concatMap (\(y, cs) -> (\(x,c) -> ((x-1,y-1), pc c)) <$> zip [0..] cs) $ zip [0..] css
    pc :: Char -> Maybe Direction
    pc '>' = Just Rt
    pc 'v' = Just Dn
    pc '^' = Just Up
    pc '<' = Just Lt
    pc _ = Nothing


inBounds :: Coord -> Bool
inBounds (x,y) = (0<=x && x<gridCols && 0<=y && y<gridRows) 
                || ((x == gridCols-1) && (y == gridRows))
                || ((x == 0) && (y == -1))


-- It's quicker to move the position backwards and compare to
-- a stationary set of blizzards
isSafe :: Snow -> Int -> Coord -> Bool
isSafe snow t (x,y) = (x, yu) `S.notMember` (snow ! Up)
                   && (x, yd) `S.notMember` (snow ! Dn)
                   && (xl, y) `S.notMember` (snow ! Lt)
                   && (xr, y) `S.notMember` (snow ! Rt)
  where
    xl = (x + t) `mod` gridCols
    xr = (x - t) `mod` gridCols
    yu = (y + t) `mod` gridRows
    yd = (y - t) `mod` gridRows


nextStates :: Snow -> State -> Set State
nextStates snow (State t pos) = S.fromList (State (t+1) <$> moves)
  where
    moves = filter (\n -> inBounds n && isSafe snow (t+1) n) $ pos : neighbours4 pos


-- Nothing special here
bfs :: (State -> Set State) -> (State -> Bool) -> State -> Maybe State
bfs next isFinished startState = go S.empty (S.singleton startState)
  where
    go !seen !q
      | null q = Nothing
      | otherwise = case S.minView q of
                      Nothing -> Nothing
                      Just (s, newq)
                        | isFinished s -> Just s
                        | ns `S.member` seen -> go seen newq
                        | otherwise -> go (ns `S.insert` seen) (S.union newq (next s))
                        where
                          ns = normalise s


-- Repeat part1 2 more times times for part 2 rather than adapting the bfs to keep going...
day24 :: IO ()
day24 = do
  ss <- getLines 24
  let snow = parseSnow ss
      finish = (gridCols-1, gridRows)
      start = (0, -1)
      s1 = fromJust $ bfs (nextStates snow) ((== finish) . pos) (State 0 start)
      s2 = fromJust $ bfs (nextStates snow) ((== start) . pos) s1
      s3 = fromJust $ bfs (nextStates snow) ((== finish) . pos) s2
      
  putStrLn $ "Day24: part1: " ++ show (time s1)
  putStrLn $ "Day24: part2: " ++ show (time s3)
  
  return ()

