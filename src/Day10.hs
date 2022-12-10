module Day10(day10) where

import Utils ( Coord, getLines )
import qualified Data.Array.IArray as A
import Data.Array.IArray (Array, (//))


parse :: String -> Ins
parse s
  | ws!!0 == "addx" = Add $ read $ ws!!1
  | otherwise = Noop
  where
    ws = words s


data Ins = Add Int | Noop deriving (Show, Eq)

add :: Ins -> Int
add (Add n) = n
add Noop = 0

isAdd :: Ins -> Bool
isAdd Noop = False
isAdd (Add _) = True

type Reg = Int
type Time = Int
type Strength = Int

type Computer = (Reg, Time)
type Screen = Array Coord Bool


rem20' :: Int -> Int
rem20' t = if t `rem` 40 == 20 then t else 0


apply' :: ([Int] -> Int -> a -> a) -> (Computer, a) -> Ins -> (Computer, a)
apply' updateFn ((reg, t), a) Noop    = ((reg, t+1), updateFn [t] reg a) 
apply' updateFn ((reg, t), a) (Add n) = ((reg + n, t+2), updateFn [t, t+1] reg a)


updateStrength :: [Int] -> Int -> Strength -> Strength
updateStrength ts r strength = strength + r * sum (rem20' <$> ts)


updateScreen :: [Int] -> Int -> Screen -> Screen
updateScreen ts spriteCol screen = screen // (amend <$> ts)
  where
    amend t = ((col0, row0), col0-1 `elem` [spriteCol-1, spriteCol, spriteCol+1])
      where
        (row0,col0) = t `quotRem` 40


render :: Screen -> String
render screen = unlines $ (\y -> (\x -> if screen A.! (x+1,y) then '#' else '.') <$> [0..39]) <$> [0..5]


day10 :: IO ()
day10 = do
  ss <- getLines 10
  --let ss = test
  let is = parse <$> ss
      arr :: Array Coord Bool
      arr = A.accumArray (const $ const False) False ((0,0), (40,6)) []

  putStrLn $ "Day10: part1: " ++ show (foldl (apply' updateStrength) ((1,1),0) is)
  putStrLn $ "Day10: part2:\n" ++ render (snd $ foldl (apply' updateScreen) ((1,1), arr) is) -- FPGPHFGH

  return ()

