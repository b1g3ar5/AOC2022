module Day3(day3) where


import Utils 
import Data.Char ( isUpper )
import Data.List ( intersect ) 


score :: Char -> Int
score c = 1 + ord c - if isUpper c then ord 'A' - 26 else ord 'a'


priority1 :: String -> Int
priority1 s = score $ head $ c1 `intersect` c2
  where
    n = length s
    (c1, c2) = splitAt (n `div` 2) s
    

priority2 :: [String] -> Int
priority2 (s1:s2:s3:_) = score $ head $ s1 `intersect` s2 `intersect` s3
priority2 _ = error "error in Day3:priority2 - there aren't 3 strings"


day3 :: IO ()
day3 = do
  ss <- getLines 3

  putStrLn $ "Day3: part1: " ++ show (sum $ priority1 <$> ss)
  putStrLn $ "Day3: part2: " ++ show (sum $ priority2 <$> chunksOf 3 ss)

  return ()
