{-# OPTIONS_GHC -Wno-orphans #-}
module Day25(day25) where

import Utils ( getLines )


type Snafu = String


toS :: Int -> Char
toS (-2) = '='
toS (-1) = '-'
toS 0 = '0'
toS 1 = '1'
toS 2 = '2'
toS i = error $ "Error can't translate toS: " ++ show i

fromS :: Char -> Int
fromS '=' = -2
fromS '-' = -1
fromS '0' = 0
fromS '1' = 1
fromS '2' = 2
fromS c = error $ "Error can't translate fromS: " ++ [c]


fromSnafu :: Snafu -> Int
fromSnafu xs = go 1 0 $ reverse $ fromS <$> xs
  where
    go :: Int -> Int -> [Int] -> Int
    go _ acc [] = acc
    go base acc (d:ds) = go (base*5) (acc + d*base) ds


-- Change quotRem so that the rem is in [-2..2]
toSnafu :: Int -> Snafu
toSnafu = reverse . go []
  where  
    go :: String -> Int -> String
    go acc n
      | q == 0 = acc ++ [toS r']
      | otherwise = go (acc ++ [toS r']) q
      where
        (q,r) = (n+2) `quotRem` 5
        r' = r-2


day25 :: IO ()
day25 = do
  ss <- getLines 25
  let g = ss

  putStrLn $ "Day25: part1: " ++ show (toSnafu $ sum $ fromSnafu <$> g)

  return ()
