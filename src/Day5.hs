module Day5(day5) where


import Utils ( getLines, pInt, parseWith, splitOn, transpose )
import Text.ParserCombinators.ReadP ( ReadP, string )


type Move = (Int, Int, Int)
type Stack = String


parse :: [String] -> ([Stack], [Move])
parse ls = (stacks, moves)
  where
    ps = splitOn " 1   2   3   4   5   6   7   8   9 " ls
    stackColumns = transpose $ head ps
    -- Extract the columns for the stacks...
    stacks = filter (/=' ') . (stackColumns!!) <$> [1, 5 .. 33]
    -- Parse the moves to be made...
    moves = parseWith parseMove <$> tail (ps!!1)


parseMove :: ReadP Move
parseMove = do
  _ <- string "move "
  x <- pInt
  _ <- string " from "
  y <- pInt
  _ <- string " to "
  z <- pInt
  return (x,y,z)


-- Slightly wasteful because it continues through the stack even when its made the move
apply :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
apply craneFn stacks (n, from, to) = go <$> zip [1..] stacks
  where
    go :: (Int, Stack) -> Stack
    go (ix, s)
      | ix == from = drop n s
      | ix == to = craneFn (take n (stacks!!(from-1))) ++ s
      | otherwise = s


day5 :: IO ()
day5 = do
  ss <- getLines 5
  let (stacks, moves) = parse ss

  putStrLn $ "Day5: part1: " ++ (head <$> foldl (apply reverse) stacks moves)
  putStrLn $ "Day5: part2: " ++ (head <$> foldl (apply id) stacks moves)

  return ()
