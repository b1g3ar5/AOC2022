module Day16(day16) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))

import Utils ( fromJust, isJust, getLines, splitOn )
import qualified Data.Set as S
import Data.Set (Set)
import Data.Functor.Foldable ( hylo )
import Data.Functor.Base ( TreeF(..), ListF(..) )
import Data.List ( tails )


type Room = String
type Time = Int
type Valve = Int
type Cave = Map Room (Maybe Valve, [Room])
data State = State {pos_::Room, time_::Time, open_::Set Room, pressure_:: Int} deriving (Eq, Show, Ord)


-- Could we unfold a useful tree?
-- Too slow - it would take about 2 hours
-- We can't prune because one branch doesn't know what the other branch is doing
buildTree :: Cave -> Int
buildTree c = hylo alg coalg $ State "AA" 15 S.empty 0
  where
    coalg :: State -> TreeF State State
    coalg s = NodeF s (nextStates c s)

    alg :: TreeF State Int -> Int
    alg (NodeF s []) = pressure_ s
    alg (NodeF _ ns) = maximum ns


buildList :: Cave -> Int
buildList c = hylo alg coalg $ State "AA" 15 S.empty 0
  where
    coalg :: State -> ListF State State
    coalg s = undefined --Cons s (nextStates c s)

    alg :: ListF State Int -> Int
    alg  = undefined 


parse :: String -> (Room, (Maybe Valve, [Room]))
parse s = (ws!!1, (if v == 0 then Nothing else Just v, splitOn ',' $ concat $ drop 9 ws))
  where
    ws = words s
    v = read $ drop 5 $ init $ ws!!4


type Pressure = Int
data State' = State' Room (Set Room) deriving (Eq, Show, Ord)


solver :: Cave -> Time -> Map (Set Room) Pressure
solver cave = go [(State' "AA" S.empty, 0)]
  where
    go :: [(State', Pressure)] -> Time -> Map (Set Room) Pressure
    go states 0 = M.fromListWith max [(open, p) | (State' _ open, p) <- states]
    go states t = go (prune (concatMap step states)) (t-1)
      where
        step :: (State', Pressure) -> [(State', Pressure)]
        step (State' room openValves,  pressure) =
            [(State' next openValves, pressure) | next <- rooms] ++
            [(State' room (S.insert room openValves), pressure + (t-1) * fromJust newPressure)
                | S.notMember room openValves
                , isJust newPressure]
            where
              (newPressure, rooms) = cave ! room
    -- This prunes all the children at each level and only keeps the fittest
    -- I can't do this in the tree because the nodes at each level don't know what 
    -- the other nodes are doing...
    prune = M.assocs . M.fromListWith max


nextStates :: Cave -> State -> [State]
nextStates c (State r t os p) 
  | t == 0 = []
  | isJust pres && r `notElem` os = openValve : ((\nr -> State nr (t-1) os p) <$> rooms)
  | otherwise = (\nr -> State nr (t-1) os p) <$> rooms
  where
    (pres, rooms) = c ! r
    openValve = State r (t-1) (r `S.insert` os) (p + fromJust pres * (t-1))


day16 :: IO ()
day16 = do
  ss <- getLines 16
  let g = M.fromList $ parse <$> ss
      routeValues2 = solver g 26
      -- This is a cheat - I just choose the best 26 minute solutions where the
      -- 2 solutions don't open the same valve
      solution2 = maximum [v1+v2 | (open1,v1) : elephants <- tails (M.assocs routeValues2),
                                   (open2,v2) <- elephants,
                                   S.null (S.intersection open1 open2)]

  putStrLn $ "Day16: part1: " ++ show (maximum $ solver g 30)
  putStrLn $ "Day16: part2: " ++ show solution2

  return ()

