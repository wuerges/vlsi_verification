module Kuelmann97 where

import Equivalence
import Graph
import BDD

import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97 :: Checker
equivKuelmann97 g1 g2 [o1] [o2] = checkExitPair result o1 o2
    where g = g1 `union` g2
          (_, _, result) = until (checkStop o1 o2) kuelmannStep (M.empty, initialInputs g, g)

-- | Checks if both outputs are mapped to the same node in rg
checkExitPair :: RG -> Int -> Int -> Maybe Bool
checkExitPair rg o1 o2 = trace ("checkExitPair: " ++ show (o1, o2)) $
  case filter (\(n, v) -> elem o1 v && elem o2 v ) (labNodes rg) of
    [only] -> Just True
    _      -> Nothing

-- | Checks if analysis should stop
checkStop :: Int -> Int -> (M.Map BDD Int, [Int], RG) -> Bool
checkStop o1 o2 (_, _, g) = maybe False (\_ -> True) (checkExitPair g o1 o2)

-- | Performs one step of the iteration
kuelmannStep :: (M.Map BDD Int, [Int], RG) -> (M.Map BDD Int, [Int], RG)
kuelmannStep (m, [], g) = (m, [] , g)
kuelmannStep (m, (i:is), g) = (m', is', g')
  where g' = maybe g (\c -> if c == i then g else mergeNodes g i c) (M.lookup bdd m)
        is' = maybe is (\ctx -> is ++ suc' ctx) (fst $ match i g')
        m' = M.insertWith (\_ o -> o) bdd i m
        bdd = createBDD g i

-- | Calculates the input nodes, that is, the nodes with input degree = 0
initialInputs :: RG -> [Int]
initialInputs g = [n | n <- nodes g, indeg g n == 0]

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessores are moved to the first one.
mergeNodes :: RG -> Int -> Int -> RG
mergeNodes g n1 n2 = error $ "uninplemented Merging nodes: " ++ show [n1, n2]
