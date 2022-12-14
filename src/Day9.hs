module Day9(day9) where

import Data.Sequence (Seq(..), singleton)
import qualified Data.Sequence as S
import Data.Set (Set)
import qualified Data.Set as Set
import Utils ( dn, getLines, lt, rt, up, Coord )


data Dir = U | D | L | R deriving (Show, Eq)


parseDir :: Char -> Coord
parseDir 'U' = up
parseDir 'D' = dn
parseDir 'L' = lt
parseDir 'R' = rt
parseDir c = error $ "Parse error: " ++ [c]


parse :: String -> (Coord, Int)
parse s = (parseDir $ s!!0, read $ tail s)


-- Parameters are (path of the tail, the snake) and (move, distance)
makeMove :: (Set Coord, Seq Coord) -> (Coord, Int) -> (Set Coord, Seq Coord)
makeMove (path, start) (mv, dist) = go path dist start
  where
    go :: Set Coord -> Int -> Seq Coord -> (Set Coord, Seq Coord)
    go acc 0 snake = (acc, snake)
    go acc n snake = go (newTail `Set.insert` acc) (n-1) newSnake
      where
        (newSnake, newTail) = moveSnake snake mv
        

-- Parameters are the snake and the move required
-- Returns the new snake and the new tail
moveSnake :: Seq Coord -> Coord -> (Seq Coord, Coord)
moveSnake Empty _ = error "There's no snake"
moveSnake (h:<|ts) mv = go (singleton $ h+mv) ts
  where
    go :: Seq Coord -> Seq Coord -> (Seq Coord, Coord)
    go az@(_ :|> z) (x :<| tts)
      | null tts = (az :|> newTail, newTail)
      | otherwise = go (az :|> newTail) tts
      where
        newTail = moveCoord z x
    go _ _ = error "Error in moveSnake"


moveCoord :: Coord -> Coord -> Coord
moveCoord front@(fx, fy) back@(bx, by)
  | (abs (fx-bx) < 2) && (abs (fy-by) < 2) = back -- touching, don't move
  | otherwise = back + signum (front - back)


day9 :: IO ()
day9 = do
  ss <- getLines 9
  let moves = parse <$> ss

  putStrLn $ "Day9: part1: " ++ show (length $ fst $ foldl makeMove (Set.empty, S.replicate 2 (0,0)) moves)
  putStrLn $ "Day9: part2: " ++ show (length $ fst $ foldl makeMove (Set.empty, S.replicate 10 (0,0)) moves)
  return ()
