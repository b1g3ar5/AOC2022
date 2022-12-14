module Day1(day1) where

import Utils ( getLines, splitOn, sort )


day1 :: IO ()
day1 = do
  ss <- getLines 1
  let elves :: [[Int]]
      elves = (read <$>) <$> splitOn "" ss

  putStrLn $ "Day1: part1: " ++ show (maximum $ sum <$> elves)
  putStrLn $ "Day1: part2: " ++ show (sum $ take 3 $ reverse $ sort $ sum <$> elves)

  return ()
