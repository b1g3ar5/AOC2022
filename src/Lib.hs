module Lib
    ( someFunc
    ) where


import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day8a
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

someFunc :: IO ()
someFunc = do 
  --day1
  --day2
  --day3
  --day4
  --day5
  --day6
  --day7
  --day8
  --day8a
  --day9
  --day10
  --day11
  --day12
  --day13
  --day14
  --day15
  --day16
  --day17
  --day18
  --day19
  --day20
  --day21
  day22
  --day23
  --day24
  --day25


makeFiles :: IO ()
makeFiles = mapM_ makeFile [1..25]

makeFile :: Int -> IO ()
makeFile n = do 
  writeFile ("./src/Day" ++ show n ++ ".hs") (proforma n)


proforma :: Int -> String
proforma n = unlines [
  "module Day" ++ show n ++ "(day" ++ show n  ++ ") where"
  , ""
  , "--import qualified Data.Set as S"
  , "--import qualified Data.Vector as V"
  , "--import qualified Data.Map.Strict as M"
  , "--import qualified Data.IntMap.Strict as IM"
  , "--import Data.Sequence (Seq(..), (><), (|>), (<|))"
  , "--import Data.List ( foldl', transpose, (\\\\), delete, group, intercalate, intersect, nub, sort, sortOn )"
  , "--import Data.List.Split (chunksOf, splitOn)"
  , "--import Data.List.Utils (replace)"
  , "--import Data.Bifunctor (Bifunctor(bimap, first, second))"
  , "--import Data.Tuple (swap)"
  , "--import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)"
  , "--import Data.Char (isAsciiLower, isAsciiUpper, toLower, toUpper, ord)"
  , "--import Control.Monad (guard)"
  , "--import Control.Monad.ST (runST, ST(..))"
  , "--import System.TimeIt ( timeIt )"
  , "--import Data.Semigroup (Semigroup(..))"
  , "--import Data.Monoid (Monoid(..))"
  , "--import Debug.Trace (trace)"
  , "--import Data.Bool (bool)"
  , "--import Data.Ord"
  , "--import Data.Function"
  , "import Utils"
  , ""
  , ""
  , "parse :: String -> Int"
  , "parse = read"
  , ""
  , ""
  , "day" ++ show n ++ " :: IO ()"
  , "day" ++ show n ++ " = do"
  , "  ss <- getLines " ++ show n ++ ""
  , "  let g = parse <$> ss"
  , ""
  , "  putStrLn $ \"Day" ++ show n ++ ": part1: \" ++ show g"
  , "  putStrLn $ \"Day" ++ show n ++ ": part2: \" ++ show \"\""
  , ""
  , "  return ()"
  ]  
