module Day3(day3) where


import Utils ( getLines )
import Data.Char ( ord, isUpper )
import Data.List.Split (chunksOf)


score :: Char -> Int
score c = 1 + ord c - if isUpper c then ord 'A' - 26 else ord 'a'


priority1 :: String -> Int
priority1 s = score c
  where
    n = length s
    (c1, c2) = splitAt (n `div` 2) s
    c = head $ filter (`elem` c1) c2
    

priority2 :: [String] -> Int
priority2 [s1,s2,s3] = score $ head f2
  where
    f1 = filter (`elem` s2) s1
    f2 = filter (`elem` s3) f1
priority2 _ = error "!!"


day3 :: IO ()
day3 = do
  ss <- getLines 3

  putStrLn $ "Day3: part1: " ++ show (sum $ priority1 <$> ss)
  putStrLn $ "Day3: part1: " ++ show (sum $ priority2 <$> chunksOf 3 ss)

  return ()
