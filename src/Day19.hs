module Day19(day19) where

import Utils hiding (bfs)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Functor.Base hiding (head, tail)
import Data.Functor.Foldable
import Control.Recursion (hyloM)
import Control.Monad.State
import qualified Data.Set as S


{-

I should go back and slove this with a bfs it would be a lot quicker
as it has much less duplication when you have a good SEEN set and
a good representation of the state.

I can't see how to sort out the duplication in the tree used in the hylomorphism.

The problem is the branches of the tree don't know what each other are doing.TQNil

Maybe I could use a list instead?

-}

data Product = ORE | CLAY | OBSIDIAN | GEODE deriving (Eq, Show, Ord)

data Blueprint = Blueprint {  oreCost::Int
                            , clayCost::Int
                            , obsidianCostOre::Int
                            , obsidianCostClay::Int
                            , geodeCostOre::Int
                            , geodeCostObsidian::Int} deriving (Show, Eq, Ord)


data Collection = Collection { ix :: Int
                   , blueprint :: Blueprint -- How much robots cost
                   , t :: Int
                   , stock :: Map Product Int -- Stock we have
                   , robots :: Map Product Int -- Robots we have
                   , allowedActions :: Map Product Bool -- Robots we are allowed to buy
                   } deriving (Eq, Ord)


instance Show Collection where
  show (Collection ix _ t s r _) = "\n" ++ show ix ++ ", " ++ show t ++ "\nStock: " ++ show (M.toList s) ++ "\nRobots: " ++ show (M.toList r)


parse :: String -> Blueprint
parse s = Blueprint (read $ ws!!6) (read $ ws!!12) (read $ ws!!18) (read $ ws!!21) (read $ ws!!27) (read $ ws!!30)
  where
    ws = words s


next :: Collection -> [Collection]
next (Collection _ _ 0 _ _ _) = []
next s@(Collection _ bp t pm rm am)
  | t==0 = error "Shouldn't get here in next" [newCollection]
  | canAfford GEODE s = [buyRobot GEODE (newCollection trueActionMap)]
  | otherwise = [buyRobot GEODE (newCollection trueActionMap) | canAfford GEODE s]
             ++ [buyRobot OBSIDIAN (newCollection trueActionMap)| canAfford OBSIDIAN s
                --, am ! OBSIDIAN
                , pm ! OBSIDIAN < maxObsidian -- stock is not too much already
                , rm ! OBSIDIAN < geodeCostObsidian bp -- not enough robots already
                ]
             ++ [buyRobot CLAY (newCollection newam)  | canAfford CLAY s
                , am ! CLAY
                , pm ! CLAY < maxClay
                , rm ! CLAY < obsidianCostClay bp
                ]
             ++ [buyRobot ORE (newCollection $ setAllowed CLAY newam) | canAfford ORE s
                , am ! ORE
                , pm ! ORE < maxOre
                , rm ! ORE < maxOreR
                ]
             ++ [newCollection newam]
  where
    newCollection = accrueMaterial s
    trueActionMap = M.fromList [(ORE, True), (CLAY, True), (OBSIDIAN, True), (GEODE, True)]
    newam = M.fromList $ (\p -> (p, not $ canAfford p s)) <$> [ORE, CLAY, OBSIDIAN, GEODE]

    -- max robots required - we only need enough robots to buy the most expensive product
    maxOreR = maximum [oreCost bp, clayCost bp, obsidianCostOre bp, geodeCostOre bp]
    maxOre = (t-1) * maxOreR
    maxClay = (t-1) * obsidianCostClay bp
    maxObsidian = (t-1) * geodeCostObsidian bp


canAfford :: Product -> Collection -> Bool
canAfford ORE s = (stock s ! ORE) >= oreCost (blueprint s)
canAfford CLAY s = (stock s ! ORE) >= clayCost (blueprint s)
canAfford OBSIDIAN s = ((stock s ! ORE) >= obsidianCostOre (blueprint s)) && ((stock s ! CLAY) >= obsidianCostClay (blueprint s))
canAfford GEODE s = ((stock s ! ORE) >= geodeCostOre (blueprint s)) && ((stock s ! OBSIDIAN) >= geodeCostObsidian (blueprint s))


buyRobot :: Product -> Collection -> Collection
buyRobot ORE (Collection ix bp t pm rm aa) = Collection ix bp t (M.adjust (+ (- oreCost bp)) ORE pm) (M.adjust (+1) ORE rm) $ setAllowed ORE aa
buyRobot CLAY (Collection ix bp t pm rm aa) = Collection ix bp t (M.adjust (+ (-clayCost bp)) ORE pm) (M.adjust (+1) CLAY rm) $ setAllowed CLAY aa
buyRobot OBSIDIAN (Collection ix bp t pm rm aa) = Collection ix bp t newpm (M.adjust (+1) OBSIDIAN rm) $ setAllowed OBSIDIAN aa
  where
    newpm = M.adjust (+ (-obsidianCostOre bp)) ORE $ M.adjust (+ (-obsidianCostClay bp)) CLAY pm
buyRobot GEODE (Collection ix bp t pm rm aa) = Collection ix bp t newpm (M.adjust (+1) GEODE rm)  $ setAllowed GEODE aa
  where
    newpm = M.adjust (+ (-geodeCostOre bp)) ORE $ M.adjust (+ (-geodeCostObsidian bp)) OBSIDIAN pm


setAllowed :: Product -> Map Product Bool -> Map Product Bool
setAllowed p = M.insert p True


-- We need to apply the new action map to finish it off
accrueMaterial :: Collection -> Map Product Bool -> Collection
accrueMaterial (Collection ix bp t pm rm _) = Collection ix bp (t-1) newpm rm
  where
    newpm = foldr (\r m -> M.adjust (+ (rm ! r)) r m) pm $ M.keys rm


initialState :: Int -> Int -> Blueprint -> Collection
initialState ix t bp = Collection ix bp t pm rm am
  where
      pm = M.fromList [(ORE, 0),(CLAY, 0),(OBSIDIAN, 0),(GEODE, 0)]
      rm = M.fromList [(ORE, 1),(CLAY, 0),(OBSIDIAN, 0),(GEODE, 0)]
      am = M.fromList [(ORE, True), (CLAY, True), (OBSIDIAN, True), (GEODE, True)]


solve1 :: Collection -> Int
solve1 s = (ix s + 1) * solve2 s

coalg :: Collection -> TreeF Collection Collection
coalg t = NodeF t $ next t

alg :: TreeF Collection Int -> Int
alg (NodeF t []) = stock t ! GEODE
alg (NodeF _ ns) = maximum ns

maxg :: Collection -> Int
maxg (Collection _ _ t pm rm _) = g + t * (t*t + 3*k*t + 3*k - 1) `div` 6
  where
    g = pm ! GEODE
    k = rm ! GEODE


-- With pruning
coalgM :: Collection -> State Int (TreeF Collection Collection)
coalgM t = do 
  savedMax <- get
  let thisMax = maxg t
      thisGeode = (! GEODE) . stock $ t
  if thisMax < savedMax 
    then 
      return $ NodeF t [] -- prune - can't beat the saved best
    else
      do
      if thisGeode > savedMax  
        then do
          put thisGeode
          return $ NodeF t $ next t
        else 
          return $ NodeF t $ next t



solve2 :: Collection -> Int
solve2 = hylo alg coalg

solve2m :: Collection -> Int
solve2m c = evalState (hyloM (return . alg) coalgM c) 0


day19 :: IO ()
day19 = do
  ss <- getLines 19
  let g24 = (\(ix, s) -> initialState ix 24 $ parse s )<$> zip [0..] ss
      g32 = take 3 $ (\(ix, s) -> initialState ix 32 $ parse s )<$> zip [0..] ss

  putStrLn $ "Day19: part1:solve " ++ show (sum $ solve1 <$> g24) -- 1613
  timeIt $ putStrLn $ "Day19: part2:solve " ++ show (product $ solve2m <$> g32) --46816
  
  return ()


{-
solve1' :: Collection -> Int
solve1' s = (ix s + 1) * solve2' s

solve2' :: Collection -> Int
solve2' = bfs next ((==0) . t)


-- Nothing special here
bfs :: (Collection -> [Collection]) -> (Collection -> Bool) -> Collection -> Int
bfs nextStates isFinished startState = go [startState]
  where
    go :: [Collection] -> Int
    go !q
      | isFinished s = maximum $ (! GEODE) . stock <$> q
      | otherwise = go newq
          where
            s = head q
            newq = concatMap nextStates q



-}


{-
data Steps a
  = Step Int (Steps a)
  | Done a
  | Fail

best :: Steps a -> Steps a -> Steps a
best Fail x = x
best x Fail = x
best (Done x) _ = Done x
best _ (Done x) = Done x
best (Step n x) (Step m y) = case compare n m of
  LT -> Step n (best x (Step (m - n) y))
  EQ -> Step n (best x y)
  GT -> Step m (best (Step (n - m) x) y)

instance Semigroup (Steps a) where
  (<>) = best

instance Monoid (Steps a) where
  mempty = Fail

-- You could alternatively return a Maybe here if you expect 
-- that you will run into cases with no successful branches.
eval :: Steps a -> a
eval (Step _ x) = eval x
eval (Done x) = x
eval Fail = error "No successful branch"

data CostTree a = Leaf a | Node Int [CostTree a]

minCost :: CostTree a -> Steps a
minCost (Leaf a) = Done a
minCost (Node n xs) = Step n (foldMap minCost xs)

-- You only have to implement the function that builds the tree:
--
-- build :: ... -> CostTree ...
-- build = ...
-- 
-- search :: ... -> ...
-- search = eval . minCost . build

-}


{-
solve2 :: State -> Int
solve2 = hylo alg coalg
  where
    coalg :: State -> TreeF State State
    coalg t = NodeF t $ next t

    alg :: TreeF State Int -> Int
    alg (NodeF t []) = stock t ! GEODE
    alg (NodeF _ ns) = maximum ns


-}

{-
solve2'' :: State -> Int
solve2'' s = maximum $ (! GEODE) . stock <$> dfs next ((==0) . t) s []


dfs :: (State -> [State]) -> (State -> Bool) -> State -> [State] -> [State]
dfs nextStates isFinished s0 visited = foldl go (visited ++ [s0]) (next s0)
  where
    go :: [State] -> State -> [State]
    go vd kid
      | isFinished kid = kid:vd
      | kid `elem` vd = vd
      | otherwise = dfs nextStates isFinished kid vd
-}
