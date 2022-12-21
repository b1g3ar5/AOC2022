module Day20(day20) where

import Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import Utils 
import Data.List ( foldl' ) 


parse :: String -> Int
parse = read

 

findWithIndex :: (a -> Bool) -> Seq a -> (Maybe (Int, a), Seq a)
findWithIndex p xs
  | isNothing mi = (Nothing , xs)
  | otherwise = (Just (ix, x), b <> f)
    where
      mi = S.findIndexL p xs
      ix = fromJust mi
      (f, x :<| b) = S.splitAt ix xs


move :: Seq (Int, Int) -> Int -> Seq (Int, Int)
move ixs n = f <> S.singleton x <> b
  where
    ((_, x@(_, v)), others) = first fromJust $ findWithIndex ((== n) . fst) ixs
    moveTo = v `mod` (length ixs -1)
    (f,b) = S.splitAt moveTo others


rotate0 :: (Eq b, Num b) => Seq (Int, b) -> Seq (Int, b)
rotate0 xs = snd (fromJust mx) :<| others
  where
    (mx, others) = findWithIndex ((==0) . snd) xs


score :: (Eq a, Num a) => Seq (Int, a) -> a
score g = gg `S.index` (1000 `mod` base) + gg `S.index` (2000 `mod` base) + gg `S.index` (3000 `mod` base)
  where 
    base = length g
    gg = snd <$> rotate0 g


grove :: Int -> Int -> Seq (Int, Int) -> Int
grove n k p = score $ go n $ second (*k) <$> p
  where
    go :: Int -> Seq (Int, Int) -> Seq (Int, Int)
    go 0 xs = xs
    go m xs = go (m-1) $ foldl' move xs [0..(length xs - 1)]


day20 :: IO ()
day20 = do
  ss <- getLines 20
  --let ss = test
  let g = parse <$> ss
      p = S.fromList $ zip [0..] g
      code = 811589153


  putStrLn $ "Day20: part1: " ++ show (grove 1 1 p)
  timeIt $ putStrLn $ "Day20: part2: " ++ show (grove 10 code p)

  return ()

