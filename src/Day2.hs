module Day2(day2) where

import Utils ( getLines )


parseLine :: String -> (Char, Char)
parseLine s = (s!!0, s!!2) 

score1 :: (Char, Char) -> Int
score1 ('A', 'X') = 4
score1 ('B', 'X') = 1
score1 ('C', 'X') = 7
score1 ('A', 'Y') = 8
score1 ('B', 'Y') = 5
score1 ('C', 'Y') = 2
score1 ('A', 'Z') = 3
score1 ('B', 'Z') = 9
score1 ('C', 'Z') = 6
score1 _ = error "!"

score2 :: (Char, Char) -> Int
score2 ('A', 'Z') = 6 + 2
score2 ('A', 'Y') = 3 + 1
score2 ('A', 'X') = 0 + 3
score2 ('B', 'Z') = 6 + 3
score2 ('B', 'Y') = 3 + 2
score2 ('B', 'X') = 0 + 1
score2 ('C', 'Z') = 6 + 1
score2 ('C', 'Y') = 3 + 3
score2 ('C', 'X') = 0 + 2
score2 _ = error "!"


day2 :: IO ()
day2 = do
  ss <- getLines 2
  let games = parseLine <$> ss

  putStrLn $ "Day2: part1: " ++ show (sum $ score1 <$> games)
  putStrLn $ "Day2: part2: " ++ show (sum $ score2 <$> games)

  return ()
