module Day13(day13) where

import Utils
import Text.ParserCombinators.ReadP ( ReadP, (+++), between, char, sepBy )
import Data.Functor ( (<&>) )


-- Can't use type and Either becasue it's a recursive type
data Item = Item Int | List [Item] deriving (Show)

comparePair :: Item -> Item -> Ordering
comparePair (Item i) (Item j) = compare i j
comparePair (List []) (List []) = EQ
comparePair (List []) (List _) = LT
comparePair (List _) (List []) = GT
comparePair (Item i) (List js) = comparePair (List [Item i]) (List js)
comparePair (List is) (Item j) = comparePair (List is) (List [Item j]) 
comparePair (List (i:is)) (List (j:js)) = case comparePair i j of 
                                              EQ -> comparePair (List is) (List js)
                                              LT -> LT
                                              GT -> GT


instance Eq Item where
  (==) :: Item -> Item -> Bool
  i1 == i2 = compare i1 i2 == EQ


instance Ord Item where
  compare :: Item -> Item -> Ordering
  compare = comparePair
parseList :: ReadP Item
parseList = between (char '[') (char ']') parseItems <&> List


parseItems :: ReadP [Item]
parseItems = sepBy parseItem (char ',')


parseItem :: ReadP Item
parseItem = (pInt <&> Item) +++ parseList


day13 :: IO ()
day13 = do
  ss <- getLines 13
  let i1, i2 :: Item
      i1 = parseWith parseList "[[2]]"
      i2 = parseWith parseList "[[6]]"
      is1, is2 :: [Item]
      is1 = parseWith parseList <$> filter (/="") ss
      is2 = sort $ is1 ++ [i1, i2]
      ix1 = 1 + fromJust (elemIndex i1 is2)
      ix2 = 1 + fromJust (elemIndex i2 is2)
      

  putStrLn $ "Day13: part1: " ++ show (sum $ (\(ix, p) -> if (p!!0) <= (p!!1) then ix else 0) <$> zip [(1::Int)..] (chunksOf 2 is1))
  putStrLn $ "Day13: part2: " ++ show (ix1*ix2)
  
  return ()


