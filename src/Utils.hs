{-# OPTIONS_GHC -Wno-orphans #-}

module Utils where

import Text.ParserCombinators.ReadP
import Data.Char


--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".in"
  return $ f s


getT :: (String -> a) -> Int -> IO a
getT f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".ex"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getTest :: Int -> IO [String]
getTest = getT lines


-- splits a list on an item and deletes the item
splitOn  :: Eq a => a -> [a] -> [[a]]
splitOn s = go [] []
  where
    go acc next [] = acc ++ [next]
    go acc next (l:ls)
      | l == s = go (acc ++ [next]) [] ls
      | otherwise = go acc (next ++ [l]) ls


parseS :: ReadP a -> ReadS a
parseS = readP_to_S


parseWith :: ReadP a -> String -> a
parseWith p s = case [ a | (a,t) <- parseS p s, all isSpace t] of
                  [a] -> a
                  [] -> error "No parse"
                  _ -> error "Ambiguous parse"


digit :: ReadP Char
digit = satisfy (`elem` "0123456789")

pInt :: ReadP Int
pInt = do
  n <- many1 digit
  return $ read n


------------------ VARIOUS UTILITY FUNCTIONS --------------------


steadyState :: Eq a => (a -> a) -> a -> a
steadyState f x = if f x == x then x else steadyState f (f x)


steadyState' :: Eq a => (a -> Maybe a) -> a -> Maybe a
steadyState' f x = case f x of
                     Nothing -> Just x
                     Just y -> steadyState' f y



------------------------ COORDINATE / VECTOR STUFF ------------


type Coord = (Int, Int)


instance Num Coord where
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger i = (fromInteger i, 0)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) +  abs (y1 - y2)


euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


neighbourCoords4 :: [Coord]
neighbourCoords4 = [(1,0), (-1,0), (0,1), (0,-1)]


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c


neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (+ origin) coords


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concat $ (\c -> (c,) <$> [0..(rows-1)]) <$> [0..(cols-1)]


directions8 :: [Coord]
directions8 = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]


directions4 :: [Coord]
directions4 = [(0, -1), (0, 1), (1, 0), (-1, 0)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)


lt, rt, up, dn :: Coord
lt = (-1,0)
rt = (1,0)
up = (0,-1)
dn = (0,1)