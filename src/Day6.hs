module Day6(day6) where

import Utils ( getLines )
import qualified Data.Set as S


findMarker :: Int -> String -> Int
findMarker markerLength s = go 0
  where
    go n 
      | S.size (S.fromList $ take markerLength $ drop n s) == markerLength = n + markerLength
      | otherwise = go (n+1)


day6 :: IO ()
day6 = do
  ss <- getLines 6
  let dat = head ss

  putStrLn $ "Day6: part1: " ++ show (findMarker 4 dat)
  putStrLn $ "Day6: part2: " ++ show (findMarker 14 dat)

  return ()
