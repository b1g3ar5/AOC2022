module Day21(day21) where

--import qualified Data.Set as S
--import qualified Data.Vector as V
--import qualified Data.Map.Strict as M
--import qualified Data.IntMap.Strict as IM
--import Data.Sequence (Seq(..), (><), (|>), (<|))
--import Data.List ( foldl', transpose, (\\), delete, group, intercalate, intersect, nub, sort, sortOn )
--import Data.List.Split (chunksOf, splitOn)
--import Data.List.Utils (replace)
--import Data.Bifunctor (Bifunctor(bimap, first, second))
--import Data.Tuple (swap)
--import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
--import Data.Char (isAsciiLower, isAsciiUpper, toLower, toUpper, ord)
--import Control.Monad (guard)
--import Control.Monad.ST (runST, ST(..))
--import System.TimeIt ( timeIt )
--import Data.Semigroup (Semigroup(..))
--import Data.Monoid (Monoid(..))
--import Debug.Trace (trace)
--import Data.Bool (bool)
--import Data.Ord
--import Data.Function
import Utils


parse :: String -> Int
parse = read


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let g = parse <$> ss

  putStrLn $ "Day21: part1: " ++ show g
  putStrLn $ "Day21: part2: " ++ show ""

  return ()
