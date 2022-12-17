{-# OPTIONS_GHC -Wno-orphans #-}

module Day12(day12) where

import Utils ( getLines, neighbours4, Coord, ord )
import Data.Array.IArray ( Array, (!), array, assocs, indices )
import qualified Search as S
import Data.Hashable
import Enumeration


type Height = Array Coord Int


instance Hashable Height where
  hashWithSalt :: Int -> Height -> Int
  hashWithSalt salt arr = hashWithSalt salt as
    where
      as = assocs arr
    

rows, cols :: Int
rows = 40
cols = 69

parse :: [String] -> Height
parse ss = array ((0,0), (rows, cols)) $ concatMap (\(r, s) -> (\(c,x) -> ((r,c), parseHeight x)) <$> zip [0..] s) $ zip [0..] ss 

parseHeight :: Char -> Int
parseHeight 'S' = 0
parseHeight 'E' = ord 'z' - ord 'a'
parseHeight c = ord c - ord 'a'


day12 :: IO ()
day12 = do
  ss <- getLines 12
  let heights = parse ss
      start, end :: (Int, Int)
      start = (20,0)
      end = (20, 46)
      
      startState :: (Coord, Height)
      startState = (start, heights)
      
      nextStates :: (Coord, Height) -> [(Coord, Height)]
      nextStates (p, h) = (,h) <$> filter (\q -> h!q <= h!p+1) (filter (`elem` indices h) (neighbours4 p))
      
      startStates :: [(Coord, Height)]
      startStates = (,heights) . fst <$> filter (\(_, x) -> x==0) (assocs heights)
      
      isFinished :: (Coord, Height) -> Bool
      isFinished (p, _) = p == end       

      toRC :: Int -> (Coord, Height)
      toRC i = (i `quotRem` (cols+1), heights)
      fromRC :: (Coord, Height) -> Int
      fromRC ((r,c), _) = r*(cols+1) + c

      enum :: Enumeration (Coord, Height)
      enum = Enumeration ((rows+1)*(cols+1)) toRC fromRC


  --timeIt $ putStrLn $ "Day12: part2: " ++ show (S.getLevel (S.bfsSlow [startState] nextStates isFinished) (end, heights))
  --timeIt $ putStrLn $ "Day12: part2: " ++ show (S.getLevel (S.bfsSlow startStates nextStates isFinished) (end, heights))
  
  putStrLn $ "Day12: part1: " ++ show (S.getLevel (S.bfs enum [startState] nextStates isFinished) (end, heights))
  putStrLn $ "Day12: part2: " ++ show (S.getLevel (S.bfs enum startStates nextStates isFinished) (end, heights))

  return ()

