{-# LANGUAGE GADTs #-}

module Day7(day7) where

import qualified Data.Map.Strict as M
import Utils ( getLines )
import Data.Either ( lefts, rights )
import Data.List ( intercalate )
import Data.Functor.Base ( TreeF(..) )
import Data.Functor.Foldable ( hylo )


type File = Int
type Directory = String

type Dmap = M.Map Directory (File, [Directory])


getDirectorySize :: Dmap -> Directory -> (Int, Bool)
getDirectorySize g target = hylo alg' coalg' (g, "")
  where
    coalg' :: (Dmap, Directory) -> TreeF (Directory, File, Bool) (Dmap, Directory)
    coalg' (mp, d) = NodeF (d, fs, d==target) $ (mp,) <$> ds
      where
        (fs, ds) = mp M.! d
    alg' :: TreeF (Directory, File, Bool) (Int, Bool) -> (Int, Bool)
    alg' (NodeF (d, fs, _) ds)
      | d == target = (fs + sum (fst <$> ds), True)
      | null ts = (fs + sum (fst <$> ds), False)
      | otherwise = head ts
      where
        ts = filter snd ds 



parse :: [String] -> Dmap
parse = parseCommand M.empty ["/"]
  where
    parseCommand ::  M.Map Directory (File, [Directory]) -> [Directory] -> [String] -> M.Map Directory (File, [Directory])
    parseCommand acc _ [] = acc
    parseCommand acc dirs (x:xs)
      | w == "$ ls" = parseCommand (M.insert dirStr (files, (\d -> dirStr ++ (if null dirStr then "" else "-") ++ d) <$> directories) acc) dirs remaining
      | w == "$ cd" = parseCommand acc (changeDir goToDir dirs) xs
      where
        dirStr = intercalate "-" $ reverse dirs
        w = take 4 x
        goToDir = drop 5 x
        (files, directories, remaining) = getFiles [] xs
        changeDir d ds
          | d == ".." = tail ds
          | d == "/" = []
          | otherwise = d:ds
        
        getFiles :: [Either File Directory] -> [String] -> (File, [Directory], [String])
        getFiles ac [] = (sum $ lefts ac, rights ac, [])
        getFiles ac (l:ls) = if head l == '$' 
                          then (sum $ lefts ac, rights ac, l:ls)
                          else getFiles (ac++[parseLine l]) ls
        
        parseLine :: String -> Either File Directory
        parseLine s
          | ws!!0 == "dir" = Right $ ws!!1
          | otherwise = Left (read $ ws!!0)
          where
            ws = words s
    parseCommand _ _ l = error $ "How did I get here?" ++ show l


day7 :: IO ()
day7 = do
  ss <- getLines 7
  let g :: Dmap
      g = parse ss
      used = maximum $ fst . getDirectorySize g <$> M.keys g
      toDelete = used + 30000000 - 70000000

  putStrLn $ "Day7: part2: " ++ show (sum $ filter (<=100000) $ fst . getDirectorySize g <$> M.keys g)
  putStrLn $ "Day7: part2: " ++ show (minimum $ filter (>=toDelete) $ fst . getDirectorySize g <$> M.keys g)
  
  return ()
