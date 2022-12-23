module Day22(day22) where

import Utils 
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List ( maximumBy, minimumBy )

-- Extremely messy solution - with a "by hand" score2 for the second part
-- Needs a rewrite with some clever vector manipulation
-- At least I use cross product for the turns
-- and I noticde that when you wrap round a corner of the cube you just have to swap
-- the face (represented as the normal) and the direction

data Cell = Wall | Path deriving (Eq, Show)

data Turn = Rt | Lt deriving (Show)
-- Face is represented by the normal to the face egs: Front = ot3, Top = up3
type Face = Coord3
type Direction = Coord3


parse2D :: [String] -> Map Coord Cell
parse2D css = M.fromList $ second fromJust  <$> ret 
  where
    ret :: [(Coord, Maybe Cell)]
    ret = filter (isJust . snd) (concatMap (\(y, cs) -> (\(x, c) -> ((x,y), pc c)) <$> zip [0..] cs) $ zip [0..] css)
    pc :: Char -> Maybe Cell
    pc ' ' = Nothing
    pc '#' = Just Wall
    pc '.' = Just Path
    pc c = error $ "Can't parse: " ++ [c]


cubeSize, maxCoord :: Int
cubeSize = 50
maxCoord = cubeSize-1


-- Takes the Coord3 for the top left and the basis vectors for the edges
parseSide :: Coord3 -> Coord3 -> Coord3 -> [String] -> [(Coord3, Cell)]
parseSide origin colinc rowinc css = concatMap 
  (\(y, cs) -> 
      (\(x, c) -> 
          (origin + scale3 y colinc + scale3 x rowinc, fromJust $ parseCell c)
      ) <$> zip [0..] cs
  ) $ zip [0..] css

{-
Done by hand from a sketch of which 
The input file is:
  FFRR
  FFRR
  DD
  DD
LLBB
LLBB
TT
TT
for FrontRight
         Down
    LeftBack
    Top
-}
parse3D :: [String] -> Map (Face, Coord3) Cell
parse3D css = M.unions [front, top, left, bottom, back, right]
  where
    rowChunks = chunksOf cubeSize $ filter (/= ' ') <$> css
    sides = concatMap (transpose . (chunksOf cubeSize <$> )) rowChunks
      
    front = M.fromList $ (first (ot3,) <$>) <$> parseSide (0,0,0) dn3 rt3 $ sides !! 0
    right = M.fromList $ (first (rt3,) <$>) <$> parseSide (maxCoord,0,0) dn3 in3 $ sides !! 1
    bottom = M.fromList $ (first (dn3,) <$>) <$> parseSide (0,maxCoord,0) in3 rt3 $ sides !! 2
    left = M.fromList $ (first (lt3,) <$>) <$> parseSide (0,maxCoord,0) up3 in3 $ sides !! 3
    back = M.fromList $ (first (in3,) <$>) <$> parseSide (0,maxCoord,maxCoord) up3 rt3 $ sides !! 4
    top = M.fromList $ (first (up3,) <$>) <$> parseSide (0,0,0) rt3 in3 $ sides !! 5


parseCell :: Char -> Maybe Cell
parseCell ' ' = Nothing
parseCell '#' = Just Wall
parseCell '.' = Just Path
parseCell c = error $ "Can't parse: " ++ [c]


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


apply2D :: Map Coord Cell -> [(Int, Turn)] -> (Coord, Coord) -> (Coord, Coord)
apply2D _ [] (pos, fac) = (pos, turn Lt fac)
apply2D mp (nt:others) pf = apply2D mp others $ go nt pf
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

score2D :: (Coord, Coord) -> Int
score2D ((x, y), d)
  | d == rt = 1000*(y+1) + 4*(x+1) + 0
  | d == dn = 1000*(y+1) + 4*(x+1) + 1
  | d == lt = 1000*(y+1) + 4*(x+1) + 2
  | d == rt = 1000*(y+1) + 4*(x+1) + 3
  | otherwise = error "Incorrect direction in score"


apply3D :: Map (Face, Coord3) Cell -> [(Int, Turn)] -> (Face, Direction, Coord3) -> (Face, Direction, Coord3)
apply3D _ [] (f, d, p) = (f, turn3 Lt (f, d), p)
apply3D mp (nt:others) fdp = apply3D mp others $ go nt fdp
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


-- This is done by hand
--   - the final dorection is (-1,0,0) = lt3
--   - for the TOP face when folded the x axis points down
--   - so lt3 points up = 3 to add for the direction...
-- To do this properly I have to work out the folding of the net of the cube
score3D :: (Face, Direction, Coord3) -> Int
score3D  (_, _, (x,_,z)) = (150 + (x+1)) * 1000 + (z+1) * 4 + 3


day22 :: IO ()
day22 = do
  ss <- getLines 22
  let mp2 = parse2D $ init $ init ss
      ins = parseIns $ last ss
      mp3 = parse3D $ init $ init ss

  putStrLn $ "Day22: part1: " ++ show (score2D $ apply2D mp2 ins ((8,0), rt)) -- 58248
  putStrLn $ "Day22: part2: " ++ show (apply3D mp3 ins (ot3, rt3, (0,0,0))) --179091
  putStrLn $ "Day22: part2: " ++ show (score3D $ apply3D mp3 ins (ot3, rt3, (0,0,0))) --179091


  return ()


--turn3 :: Turn -> (Face, Direction) -> Direction
--turn3 Lt (f,d) = crossProduct (normal f) d
--turn3 Rt (f,d) = scale3 (-1) $ crossProduct (normal f) d

--wrap3 :: (Face, Direction) -> (Face, Direction)
--wrap3 (f, d) = (unnormal d, scale3 (-1) $ normal f)

turn3 :: Turn -> (Face, Direction) -> Direction
turn3 Lt (f,d) = crossProduct f d
turn3 Rt (f,d) = scale3 (-1) $ crossProduct f d

wrap3 :: (Face, Direction) -> (Face, Direction)
wrap3 (f, d) = (d, scale3 (-1) f)

