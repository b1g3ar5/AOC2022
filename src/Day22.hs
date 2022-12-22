module Day22(day22) where

import Utils 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List ( maximumBy, minimumBy )

-- Extremely messy solution - with a "by hand" score2 for the second part
-- Needs a rewrite with some clever vector manipulation - cross products etc.

data Cell = Wall | Path deriving (Eq, Show)

parseMap :: [String] -> Map Coord Cell
parseMap css = M.fromList $ second fromJust  <$> ret 
  where
    ret :: [(Coord, Maybe Cell)]
    ret = filter (isJust . snd) (concatMap (\(y, cs) -> (\(x, c) -> ((x,y), pc c)) <$> zip [0..] cs) $ zip [0..] css)
    pc :: Char -> Maybe Cell
    pc ' ' = Nothing
    pc '#' = Just Wall
    pc '.' = Just Path
    pc c = error $ "Can't parse: " ++ [c]

cubeSize = 50

parseSide :: Coord3 -> Coord3 -> Coord3 -> [String] -> [(Coord3, Cell)]
parseSide origin colinc rowinc css = concatMap 
  (\(y, cs) -> 
      (\(x, c) -> 
          (origin + scale3 y colinc + scale3 x rowinc, fromJust $ parseCell c)
      ) <$> zip [0..] cs
  ) $ zip [0..] css


--parseMap3 :: [String] -> [Map Coord3 Cell]
--parseMap3 :: [[Char]] -> [Coord3 -> [String] -> Map Coord3 Cell]
parseMap3 css = M.unions [front, top, left, bottom, back, right]
  where
    rowChunks = chunksOf cubeSize $ filter (/= ' ') <$> css
    sides = concat $ transpose . (chunksOf cubeSize <$> ) <$> rowChunks
      
    front = M.fromList $ (first (Front,) <$>) <$> parseSide (0,0,0) dn3 rt3 $ sides !! 0
    top = M.fromList $ (first (Top,) <$>) <$> parseSide (3,0,0) in3 lt3 $ sides !! 1
    left = M.fromList $ (first (Left_,) <$>) <$> parseSide (0,0,0) in3 dn3 $ sides !! 2
    bottom = M.fromList $ (first (Bottom,) <$>) <$> parseSide (0,3,0) in3 rt3 $ sides !! 3
    back = M.fromList $ (first (Back,) <$>) <$> parseSide (0,3,3) up3 rt3 $ sides !! 4
    right = M.fromList $ (first (Right_,) <$>) <$> parseSide (3,3,3) up3 ot3 $ sides !! 5
    

parseMap3' css = M.unions [front, top, left, bottom, back, right]
  where
    rowChunks = chunksOf cubeSize $ filter (/= ' ') <$> css
    sides = concat $ transpose . (chunksOf cubeSize <$> ) <$> rowChunks
      
    front = M.fromList $ (first (Front,) <$>) <$> parseSide (0,0,0) dn3 rt3 $ sides !! 0
    right = M.fromList $ (first (Right_,) <$>) <$> parseSide (49,0,0) dn3 in3 $ sides !! 1
    bottom = M.fromList $ (first (Bottom,) <$>) <$> parseSide (0,49,0) in3 rt3 $ sides !! 2
    left = M.fromList $ (first (Left_,) <$>) <$> parseSide (0,49,0) up3 in3 $ sides !! 3
    back = M.fromList $ (first (Back,) <$>) <$> parseSide (0,49,49) up3 rt3 $ sides !! 4
    top = M.fromList $ (first (Top,) <$>) <$> parseSide (0,0,0) rt3 in3 $ sides !! 5


parseCell :: Char -> Maybe Cell
parseCell ' ' = Nothing
parseCell '#' = Just Wall
parseCell '.' = Just Path
parseCell c = error $ "Can't parse: " ++ [c]



data Turn = Rt | Lt deriving (Show)
data Face = Front | Back | Top | Bottom | Left_ | Right_ deriving (Show, Eq, Ord)
type Direction = Coord3


parseTurn :: Char -> Turn
parseTurn 'R' = Rt
parseTurn 'L' = Lt
parseTurn c = error $ "Can't parse turn: " ++ [c]


parseIns  :: String -> [(Int, Turn)]
parseIns s = second parseTurn <$> zip ns ls
  where
    ns = read <$> splitWhen (\c -> c=='R' || c=='L') s
    ls = filter  (\c -> c=='R' || c=='L') s

turn :: Turn -> Coord -> Coord
turn Lt c
  | c==up = lt
  | c==dn = rt
  | c==lt = dn
  | c==rt = up
  | otherwise = error $ "Error in turn with: " ++ show c
turn Rt c
  | c==up = rt
  | c==dn = lt
  | c==lt = up
  | c==rt = dn
  | otherwise = error $ "Error in turn with: " ++ show c


apply :: Map Coord Cell -> [(Int, Turn)] -> (Coord, Coord) -> (Coord, Coord)
apply _ [] (pos, fac) = (pos, turn Lt fac)
apply mp (nt:others) pf = apply mp others $ go nt pf
  where
    go (0,t) (p, f) = (p, turn t f)
    go (n,t) (p, f) = maybe (p, turn t f) (\nxt -> go (n-1, t) (nxt, f)) $ next mp (p, f)


wrap :: (Coord, Coord) -> [Coord] -> Coord
wrap ((x,y), fac) cs
  | fac == lt = maximumBy (compare `on` fst) $ filter ((==y) . snd) cs
  | fac == rt = minimumBy (compare `on` fst) $ filter ((==y) . snd) cs
  | fac == up = maximumBy (compare `on` snd) $ filter ((==x) . fst) cs
  | fac == dn = minimumBy (compare `on` snd) $ filter ((==x) . fst) cs
  | otherwise = error $ "Can't wrap: " ++ show fac


-- Get next cell
next :: Map Coord Cell -> (Coord, Coord) -> Maybe Coord
next mp (pos, fac)
  | isNothing (mp M.!? (pos+fac)) = if mp M.! wn == Wall then Nothing else Just wn
  | mp M.! (pos+fac) == Wall = Nothing
  | otherwise = Just $ pos + fac
  where
    wn = wrap (pos, fac) (M.keys mp)

score :: (Coord, Coord) -> Int
score ((x, y), d)
  | d == rt = 1000*(y+1) + 4*(x+1) + 0
  | d == dn = 1000*(y+1) + 4*(x+1) + 1
  | d == lt = 1000*(y+1) + 4*(x+1) + 2
  | d == rt = 1000*(y+1) + 4*(x+1) + 3
  | otherwise = error "Incorrect direction in score"


apply2 :: Map (Face, Coord3) Cell -> [(Int, Turn)] -> (Face, Direction, Coord3) -> (Face, Direction, Coord3)
apply2 _ [] fdp = fdp
apply2 mp (nt:others) fdp = apply2 mp others $ go nt fdp
  where
    go :: (Int, Turn) -> (Face, Direction, Coord3) -> (Face, Direction, Coord3)
    go (0,t) (f, d, p) = (f, turn3 t (f, d), p)
    go (n,t) (f, d, p) = case mp M.!? (f, p+d) of
                       Nothing -> case mp M.!? (fst $ wrap3 (f, d), p) of
                                    Nothing -> error $ "Error in case : " ++ show (f,d,p)
                                    Just Path ->  go (n-1,t) $ (\(nf, nd) -> (nf, nd, p))  $ wrap3 (f, d) 
                                    Just Wall -> (f, turn3 t (f,d), p)
                       Just Wall -> (f, turn3 t (f,d), p)
                       Just Path -> go (n-1, t) (f, d, p+d)


score2 :: (Face, Direction, Coord3) -> Int
score2  (_, _, (x,_,z)) = (150 + (x+1)) * 1000 + (z+1) * 4 + 3


day22 :: IO ()
day22 = do
  ss <- getLines 22
  let mp = parseMap $ init $ init ss
      ins = parseIns $ last ss
      mp3 = parseMap3' $ init $ init ss

  putStrLn $ "Day22: part1: " ++ show (score $ apply mp ins ((8,0), rt))
  putStrLn $ "Day22: part2: " ++ show (score2 $ apply2 mp3 ins (Front, rt3, (0,0,0)))
  
  return ()

turn3 :: Turn -> (Face, Direction) -> Direction
turn3 Lt (Front, d)
  | d == rt3 = up3
  | d == lt3 = dn3
  | d == up3 = lt3
  | d == dn3 = rt3
  | otherwise = error "Incorrect direction in turn3"
turn3 Lt (Back, d)
  | d == rt3 = dn3
  | d == lt3 = up3
  | d == up3 = rt3
  | d == dn3 = lt3
  | otherwise = error "Incorrect direction in turn3"
turn3 Lt (Left_, d)
  | d == in3 = dn3
  | d == ot3 = up3
  | d == up3 = in3
  | d == dn3 = ot3
  | otherwise = error "Incorrect direction in turn3"
turn3 Lt (Right_, d)
  | d == in3 = up3
  | d == ot3 = dn3
  | d == up3 = ot3
  | d == dn3 = in3
  | otherwise = error "Incorrect direction in turn3"
turn3 Lt (Top, d)
  | d == in3 = lt3
  | d == ot3 = rt3
  | d == lt3 = ot3
  | d == rt3 = in3
  | otherwise = error "Incorrect direction in turn3"
turn3 Lt (Bottom, d)
  | d == in3 = rt3
  | d == ot3 = lt3
  | d == lt3 = in3
  | d == rt3 = ot3
  | otherwise = error "Incorrect direction in turn3"
turn3 Rt fd = scale3 (-1) $ turn3 Lt fd


wrap3 :: (Face, Direction) -> (Face, Direction)
wrap3 (Front, d)
  | d == rt3 = (Right_, in3)
  | d == lt3 = (Left_, in3)
  | d == up3 = (Top, in3)
  | d == dn3 = (Bottom, in3)
  | otherwise = error "Incorrect direction in turn3"
wrap3 (Back, d)
  | d == rt3 = (Right_, ot3)
  | d == lt3 = (Left_, ot3)
  | d == up3 = (Top, ot3)
  | d == dn3 = (Bottom, ot3)
  | otherwise = error "Incorrect direction in turn3"
wrap3 (Left_, d)
  | d == in3 = (Back, rt3)
  | d == ot3 = (Front, rt3)
  | d == up3 = (Top, rt3)
  | d == dn3 = (Bottom, rt3)
  | otherwise = error "Incorrect direction in turn3"
wrap3 (Right_, d)
  | d == in3 = (Back, lt3)
  | d == ot3 = (Front, lt3)
  | d == up3 = (Top, lt3)
  | d == dn3 = (Bottom, lt3)
  | otherwise = error "Incorrect direction in turn3"
wrap3 (Top, d)
  | d == in3 = (Back, dn3)
  | d == ot3 = (Front, dn3)
  | d == lt3 = (Left_, dn3)
  | d == rt3 = (Right_, dn3)
  | otherwise = error "Incorrect direction in turn3"
wrap3 (Bottom, d)
  | d == in3 = (Back, up3)
  | d == ot3 = (Front, up3)
  | d == lt3 = (Left_, up3)
  | d == rt3 = (Right_, up3)
  | otherwise = error "Incorrect direction in turn3"

