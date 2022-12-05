module Day4(day4) where

import Utils ( getLines, parseWith, pInt ) 
import Text.ParserCombinators.ReadP ( ReadP, get )


-- A proper parser today - practice for later
parse :: ReadP ((Int, Int),(Int,Int))
parse = do
  n1 <- pInt
  _ <- get -- the '-'
  n2 <- pInt
  _ <- get -- the ','
  m1 <- pInt
  _ <- get -- another '-' 
  m2 <- pInt
  return ((n1, n2),(m1, m2))


contained :: Ord a => ((a,a),(a,a)) -> Bool
contained ((m1, m2), (n1,n2)) = (n1>=m1 && n2<=m2) || (m1>=n1 && m2<=n2)


-- No overlap is easier to define because the numbers (in a tupple) are ordered
overlap :: Ord a => ((a, a), (a, a)) -> Bool
overlap ((m1, m2), (n1,n2)) = not (m1>n2 || m2<n1) 


day4 :: IO ()
day4 = do
  ss <- getLines 4
  let ps = parseWith parse <$> ss

  putStrLn $ "Day4: part1: " ++ show (length $ filter id $ contained <$> ps)
  putStrLn $ "Day4: part2: " ++ show (length $ filter id $ overlap <$> ps)
  
  return ()
