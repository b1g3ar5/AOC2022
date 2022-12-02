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
import Day9
import Day10

someFunc :: IO ()
someFunc = do 
  day1
  day2


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
