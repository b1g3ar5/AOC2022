module Day11(day11) where

import Utils ( getLines, sort, chunksOf )
import qualified Data.IntMap.Strict as IM

type Item = Int

data Monkey = Monkey {inspections :: Int , items :: [Item], op :: Item -> Item, divtest :: Int, tf :: (Int, Int)}


parseList :: String -> [Int]
parseList s = read <$> ws
  where
    ws = words $ filter (/=',') s


parseOp :: String -> String -> (Int ->Int)
parseOp op n 
  | n=="old" = \x -> x * x -- not robust!
  | op=="+" = \x -> x + read n
  | op=="*" = \x -> x * read n
  | otherwise = error $ "Need to parse: " ++ op ++ " " ++ n


-- Very fragile parsing...
parseMonkey :: [String] -> (Int, Monkey)
parseMonkey ls = (read $ init $ words (ls!!0) !! 1
                 , Monkey 0 (parseList $ drop 18 $ ls!!1) 
                            (parseOp (ws2!!0) (ws2!!1))
                            (read $ drop 21 $ ls!!3)
                            (read $ drop 29 $ ls!!4, read $ drop 29 $ ls!!5)
                 )
  where
    ws2 = words $ drop 23 $ ls!!2
    

rounds :: Int -> Int -> IM.IntMap Monkey -> IM.IntMap Monkey
rounds _ 0 ms = ms
rounds divisor n ms = rounds divisor (n-1) $ runRound divisor ms
    

runRound :: Int -> IM.IntMap Monkey -> IM.IntMap Monkey
runRound divisor ms = foldl (monkey divisor) ms [0..7]


monkey :: Int -> IM.IntMap Monkey -> Int -> IM.IntMap Monkey
monkey divisor ms from = go ms $ items $ ms IM.! from 
  where
    go :: IM.IntMap Monkey -> [Item] -> IM.IntMap Monkey
    go monkeys [] = monkeys
    go monkeys (i:items) = go (throw newi from to monkeys) items
      where
        monk = ms IM.! from
        newi = op monk i `div` divisor `rem` base
        to = if newi `rem` divtest monk == 0 then fst $ tf monk else snd $ tf monk


throw :: Int -> Int -> Int -> IM.IntMap Monkey -> IM.IntMap Monkey
throw item from to ms = IM.adjust (\(Monkey m is a b c) -> Monkey (m+1) (tail is) a b c ) from 
                      $ IM.adjust (\(Monkey n is a b c) -> Monkey n (is ++ [item]) a b c ) to ms


-- Product of all the divisors in the monkey predicates
-- This will not change the remainders
base :: Int
base = 2*7*13*5*3*19*11*17
         
day11 :: IO ()
day11 = do
  ss <- getLines 11
  let ms = IM.fromList $ parseMonkey <$> chunksOf 7 ss

  putStrLn $ "Day11: part1: " ++ show (product $ take 2 $ reverse $ sort $ inspections <$> IM.elems (rounds 3 20 ms))
  putStrLn $ "Day11: part2: " ++ show (product $ take 2 $ reverse $ sort $ inspections <$> IM.elems (rounds 1 10000 ms))

  return ()


