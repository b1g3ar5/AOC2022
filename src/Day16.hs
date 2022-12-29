module Day16(day16) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Utils ( fromJust, isJust, getLines, splitOn, timeIt )
import qualified Data.Set as S
import Data.Set (Set)
import Data.List ( tails )


type Room = String
type Time = Int
type Valve = Int
type Cave = Map Room (Maybe Valve, [Room])
type Pressure = Int
data State' = State' {r::Room, sr:: Set Room} deriving (Eq, Show, Ord)


parse :: String -> (Room, (Maybe Valve, [Room]))
parse s = (ws!!1, (if v == 0 then Nothing else Just v, splitOn ',' $ concat $ drop 9 ws))
  where
    ws = words s
    v = read $ drop 5 $ init $ ws!!4


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
    prune = M.assocs . M.fromListWith max


day16 :: IO ()
day16 = do
  ss <- getLines 16
  let g = M.fromList $ parse <$> ss
      routeValues2 = solver g 26
      -- This is a cheat - I just choose the best 26 minute solutions where the
      -- 2 solutions don't open the same valve. This doesn't waste any valve openings
      -- and it means we can add the pressures.
      solution2 = maximum [v1+v2 | (open1,v1) : elephants <- tails (M.assocs routeValues2),
                                   (open2,v2) <- elephants,
                                   S.null (S.intersection open1 open2)]

  timeIt $ putStrLn $ "Day16: part1: " ++ show (maximum $ solver g 30)
  timeIt $ putStrLn $ "Day16: part2: " ++ show solution2

  return ()

