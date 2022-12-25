module Day21(day21) where

import Utils
import Data.Map.Strict 
import Data.Functor.Base hiding (head)
import Data.Functor.Foldable


data Expr = Var Int 
            | Eq String String
            | Add String String 
            | Sub String String 
            | Mul String String 
            | Div String String deriving (Eq, Show)


parse :: String -> (String, Expr)
parse s 
  | nw == 2 =  (init (ws!!0), Var $ read $ ws!!1)
  | ws!!2 == "+" = (init (ws!!0), Add (ws!!1) (ws!!3))
  | ws!!2 == "-" = (init (ws!!0), Sub (ws!!1) (ws!!3))
  | ws!!2 == "*" = (init (ws!!0), Mul (ws!!1) (ws!!3))
  | ws!!2 == "/" = (init (ws!!0), Div (ws!!1) (ws!!3))
  | otherwise = error $ "Can't parse: " ++ s
  where
    ws = words s
    nw = length ws


eval :: String -> Map String Expr -> Int
eval v es = case es ! v of
              Var n -> n
              Add v1 v2 -> eval v1 es + eval v2 es
              Sub v1 v2 -> eval v1 es - eval v2 es
              Mul v1 v2 -> eval v1 es * eval v2 es
              Div v1 v2 -> eval v1 es `div` eval v2 es
              Eq v1 v2 -> trace (show (eval v1 es) ++ ", " ++ show (eval v2 es)) $ if eval v1 es == eval v2 es then 1 else 0


coalg :: (String, Map String Expr) -> TreeF (String, Int -> Int -> Int, Char) (String, Map String Expr)
coalg (s, mp) = case e of 
                  Var n -> NodeF (s,\_ _ -> n,'_') []
                  Add x y -> NodeF (s,(+),'+') [(x, mp), (y, mp)]
                  Sub x y -> NodeF (s,(-),'-') [(x, mp), (y, mp)]
                  Mul x y -> NodeF (s,(*),'*') [(x, mp), (y, mp)]
                  Div x y -> NodeF (s,div,'/') [(x, mp), (y, mp)]
                  _ -> error $ "Can't add: " ++ (s ++ ": " ++ show e) ++ " to the tree"
  where
    e = mp ! s


alg1 :: TreeF (String, Int -> Int -> Int, Char) Int -> Int
alg1 (NodeF (_, op, _) []) = op 0 0
alg1 (NodeF (_, op, _) ks) = op (ks!!0) (ks!!1)


alg2 :: TreeF (String, Int -> Int -> Int, Char) (Either (Int -> Int) Int) -> Either (Int->Int) Int
alg2 (NodeF ("humn", _, _) _) = Left id
alg2 (NodeF ("root", _, _) ks) = case (ks!!0, ks!!1) of 
                                 (Left f1, Right x2) -> Right $ f1 x2
                                 (Right x1, Left f2) -> Right $ f2 x1
                                 (Left _, Left _) -> error "Too many humans"
                                 (Right _, Right _) -> error "Too few humans"
alg2 (NodeF (_, op, _) []) = Right $ op 0 0
alg2 (NodeF (_, _, '+') ks) = case (ks!!0, ks!!1) of 
                                 (Right x1, Right x2) -> Right $ x1+x2
                                 (Left f1, Right x2) -> Left $ \t -> f1 $ t - x2
                                 (Right x1, Left f2) -> Left $ \t -> f2 $ t - x1
                                 _  -> error "Too many humans"
alg2 (NodeF (_, _, '*') ks) = case (ks!!0, ks!!1) of 
                                 (Right x1, Right x2) -> Right $ x1*x2
                                 (Left f1, Right x2) -> Left $ \t -> f1 $ t `div` x2
                                 (Right x1, Left f2) -> Left $ \t -> f2 $ t `div` x1
                                 _ -> error "Too many humans"
alg2 (NodeF (_, _, '-') ks) = case (ks!!0, ks!!1) of 
                                 (Right x1, Right x2) -> Right $ x1-x2
                                 (Left f1, Right x2) -> Left $ \t -> f1 $ x2 + t
                                 (Right x1, Left f2) -> Left $ \t -> f2 $ x1 - t
                                 _ -> error "Too many humans"
alg2 (NodeF (_, _, '/') ks) = case (ks!!0, ks!!1) of 
                                 (Right x1, Right x2) -> Right $ x1 `div` x2
                                 (Left f1, Right x2) -> Left $ \t -> f1 $ x2 * t
                                 (Right x1, Left f2) -> Left $ \t -> f2 $ x1 `div` t
                                 _  -> error "Too many humans"
alg2 (NodeF (_, _, c) _) = error $ "Unknown operation: " ++ [c]


newton :: Map String Expr -> Int
newton mp = go 100
  where
    go guess
      | f == 0 = guess
      | otherwise = go $ guess - f `div` df
      where
        f = eval "root" $ insert "humn" (Var guess) mp
        fup = eval "root" $ insert "humn" (Var $ guess+1) mp
        df = fup - f


day21 :: IO ()
day21 = do
  ss <- getLines 21
  let g = fromList $ parse <$> ss
      gg = insert "humn" (Var 0) $ insert  "root" (Sub "lvvf" "rqgq")  g

  putStrLn $ "Day21: part1: " ++ show (eval "root" g)
  putStrLn $ "Day21: part1:hylo " ++ show (hylo alg1 coalg ("root", g))
  putStrLn $ "Day21: part2: " ++ show (newton gg)
  putStrLn $ "Day21: part2:hylo " ++ show (head $ rights [hylo alg2 coalg ("root", g)])

  return ()
