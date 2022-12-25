module Day19(day19) where

import Utils hiding (bfs)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Functor.Base hiding (head)
import Data.Functor.Foldable
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


data State = State { ix :: Int
                   , blueprint :: Blueprint -- How much robots cost
                   , t :: Int
                   , stock :: Map Product Int -- Stock we have
                   , robots :: Map Product Int -- Robots we have
                   , allowedActions :: Map Product Bool
                   } deriving (Eq)

instance Ord State where
  s1 <= s2 = t s1 <= t s2

rep :: State -> (Int, [Int], [Int])
rep s = (t s, M.elems $ robots s, M.elems $ stock s)


instance Show State where
  show (State ix _ t s r _) = "\n" ++ show ix ++ ", " ++ show t ++ "\nStock: " ++ show (M.toList s) ++ "\nRobots: " ++ show (M.toList r)


parse :: String -> Blueprint
parse s = Blueprint (read $ ws!!6) (read $ ws!!12) (read $ ws!!18) (read $ ws!!21) (read $ ws!!27) (read $ ws!!30)
  where
    ws = words s


solve1 :: State -> Int
solve1 s = (ix s + 1) * solve2 s 


solve2 :: State -> Int
solve2 = hylo alg coalg
  where
    coalg :: State -> TreeF State State
    coalg t = NodeF t $ next t

    alg :: TreeF State Int -> Int
    alg (NodeF t []) = stock t ! GEODE
    alg (NodeF _ ns) = maximum ns    


next :: State -> [State]
next (State _ _ 0 _ _ _) = []
next s@(State _ bp t pm rm am)
  | t==0 = error "Shouldn't get here in next" [newState]
  | canAfford GEODE s = [buyRobot GEODE (newState trueActionMap)]
  | otherwise = [buyRobot GEODE (newState trueActionMap) | canAfford GEODE s]
             ++ [buyRobot OBSIDIAN (newState trueActionMap)| canAfford OBSIDIAN s
                --, am ! OBSIDIAN
                , pm ! OBSIDIAN < maxObsidian -- stock is not too much already
                , rm ! OBSIDIAN < geodeCostObsidian bp -- not enough robots already
                ]
             ++ [buyRobot CLAY (newState newam)  | canAfford CLAY s
                , am ! CLAY
                , pm ! CLAY < maxClay
                , rm ! CLAY < obsidianCostClay bp
                ]
             ++ [buyRobot ORE (newState $ setAllowed CLAY newam) | canAfford ORE s
                , am ! ORE
                , pm ! ORE < maxOre
                , rm ! ORE < maxOreR
                ]
             ++ [newState newam]
  where
    newState = accrueMaterial s
    trueActionMap = M.fromList [(ORE, True), (CLAY, True), (OBSIDIAN, True), (GEODE, True)]
    newam = M.fromList $ (\p -> (p, not $ canAfford p s)) <$> [ORE, CLAY, OBSIDIAN, GEODE]

    -- max robots required - we only need enough robots to buy the most expensive product
    maxOreR = maximum [oreCost bp, clayCost bp, obsidianCostOre bp, geodeCostOre bp]
    maxOre = (t-1) * maxOreR
    maxClay = (t-1) * obsidianCostClay bp
    maxObsidian = (t-1) * geodeCostObsidian bp


canAfford :: Product -> State -> Bool
canAfford ORE s = (stock s ! ORE) >= oreCost (blueprint s)
canAfford CLAY s = (stock s ! ORE) >= clayCost (blueprint s)
canAfford OBSIDIAN s = ((stock s ! ORE) >= obsidianCostOre (blueprint s)) && ((stock s ! CLAY) >= obsidianCostClay (blueprint s))
canAfford GEODE s = ((stock s ! ORE) >= geodeCostOre (blueprint s)) && ((stock s ! OBSIDIAN) >= geodeCostObsidian (blueprint s))


buyRobot :: Product -> State -> State
buyRobot ORE (State ix bp t pm rm aa) = State ix bp t (M.adjust (+ (- oreCost bp)) ORE pm) (M.adjust (+1) ORE rm) $ setAllowed ORE aa
buyRobot CLAY (State ix bp t pm rm aa) = State ix bp t (M.adjust (+ (-clayCost bp)) ORE pm) (M.adjust (+1) CLAY rm) $ setAllowed CLAY aa
buyRobot OBSIDIAN (State ix bp t pm rm aa) = State ix bp t newpm (M.adjust (+1) OBSIDIAN rm) $ setAllowed OBSIDIAN aa
  where
    newpm = M.adjust (+ (-obsidianCostOre bp)) ORE $ M.adjust (+ (-obsidianCostClay bp)) CLAY pm
buyRobot GEODE (State ix bp t pm rm aa) = State ix bp t newpm (M.adjust (+1) GEODE rm)  $ setAllowed GEODE aa
  where
    newpm = M.adjust (+ (-geodeCostOre bp)) ORE $ M.adjust (+ (-geodeCostObsidian bp)) OBSIDIAN pm


setAllowed :: Product -> Map Product Bool -> Map Product Bool
setAllowed p = M.insert p True


-- We need to apply the new action map to finish it off
accrueMaterial :: State -> Map Product Bool -> State
accrueMaterial (State ix bp t pm rm _) = State ix bp (t-1) newpm rm
  where
    newpm = foldr (\r m -> M.adjust (+ (rm ! r)) r m) pm $ M.keys rm


makeState :: Int -> Int -> Blueprint -> State
makeState ix t bp = State ix bp t pm rm am
  where
      pm = M.fromList [(ORE, 0),(CLAY, 0),(OBSIDIAN, 0),(GEODE, 0)]
      rm = M.fromList [(ORE, 1),(CLAY, 0),(OBSIDIAN, 0),(GEODE, 0)]
      am = M.fromList [(ORE, True), (CLAY, True), (OBSIDIAN, True), (GEODE, True)]


day19 :: IO ()
day19 = do
  ss <- getLines 19
  let g24 = (\(ix, s) -> makeState ix 24 $ parse s )<$> zip [0..] ss
      g32 = take 3 $ (\(ix, s) -> makeState ix 32 $ parse s )<$> zip [0..] ss
   
  timeIt $ putStrLn $ "Day19: part1:solve " ++ show (sum $ solve1 <$> g24) -- 1613
  timeIt $ putStrLn $ "Day19: part2:solve " ++ show (solve2 <$> g32) --46816

  return ()

