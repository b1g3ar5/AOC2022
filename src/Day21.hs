module Day21(day21) where

import Utils
import Data.Map.Strict 
import Data.Functor.Base
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


coalg :: (String, Map String Expr) -> TreeF (Int -> Int -> Int) (String, Map String Expr)
coalg (s, mp) = case e of 
                  Var n -> NodeF (\_ _ -> n) []
                  Add x y -> NodeF (+) [(x, mp), (y, mp)]
                  Sub x y -> NodeF (-) [(x, mp), (y, mp)]
                  Mul x y -> NodeF (*) [(x, mp), (y, mp)]
                  Div x y -> NodeF div [(x, mp), (y, mp)]
                  _ -> error $ "Can't add: " ++ (s ++ ": " ++ show e) ++ " to the tree"
  where
    e = mp ! s

alg :: TreeF (Int -> Int -> Int) Int -> Int
alg (NodeF op []) = op 0 0
alg (NodeF op ks) = op (ks!!0) (ks!!1)


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
  putStrLn $ "Day21: part1: " ++ show (hylo alg coalg ("root", g))
  putStrLn $ "Day21: part1: " ++ show (newton gg)

  return ()

test = ["root: pppw + sjmn"
  , "dbpl: 5"
  , "cczh: sllz + lgvd"
  , "zczc: 2"
  , "ptdq: humn - dvpt"
  , "dvpt: 3"
  , "lfqf: 4"
  , "humn: 5"
  , "ljgn: 2"
  , "sjmn: drzm * dbpl"
  , "sllz: 4"
  , "pppw: cczh / lfqf"
  , "lgvd: ljgn * ptdq"
  , "drzm: hmdt - zczc"
  , "hmdt: 32"]