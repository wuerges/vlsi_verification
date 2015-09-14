module Kuelmann97 where

import Equivalence
import Graph
import BDD

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97 :: Checker
equivKuelmann97 g1 g2 [o1] [o2] = checkExitPair result o1 o2
    where g = g1 `union` g2
          (_, _, result) = until (checkStop o1 o2) kuelmannStep (m0, is0, g)
          m0 = initialMap g is0
          is0 = initialInputs g

-- | Checks if both outputs are mapped to the same node in rg
checkExitPair :: RG -> Int -> Int -> Maybe Bool
checkExitPair rg o1 o2 = --trace ("checkExitPair: " ++ show (o1, o2)) $
  case filter (\(n, v) -> elem o1 v && elem o2 v ) (labNodes rg) of
    [only] -> Just True
    _      -> Nothing

-- | Checks if analysis should stop
checkStop :: Int -> Int -> (M.Map BDD Int, [Int], RG) -> Bool
checkStop o1 o2 (_, [], g) = False
checkStop o1 o2 (_, _, g) = maybe False (\_ -> True) (checkExitPair g o1 o2)

-- | Performs one step of the iteration
kuelmannStep :: (M.Map BDD Int, [Int], RG) -> (M.Map BDD Int, [Int], RG)
kuelmannStep (m, [], g) = (m, [] , g)
kuelmannStep (m, (i:is), g) | memberNode i g = --traceShow (m', is', g')
                                (m', is', g')
                            | otherwise      = (m, is, g)
  where (m', g') = case M.lookup bdd m of
                          Just c -> (m, if c == i then g else mergeNodes g i c)
                          Nothing -> (M.insert bdd i m, g)
        --g' = maybe g (\c -> if c == i then g else mergeNodes g i c) (M.lookup bdd m)
        is' = nub $ is ++ suc g i
        --m' = M.insertWith (\_ o -> o) bdd i m
        bdd = createBDD g i

memberNode n g = maybe False (\_ -> True) (fst $ match n g)

-- | Calculates the input nodes, that is, the nodes with input degree = 0
initialInputs :: RG -> [Int]
initialInputs g = [n | n <- nodes g, indeg g n == 0]

initialMap :: RG -> [Int] -> M.Map BDD Int
initialMap g is = foldl (\m i -> M.insert (createBDD g i) i m) M.empty is

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessores are moved to the first one.
mergeNodes :: RG -> Int -> Int -> RG
mergeNodes g n1 n2 = case match n2 g of
        (Just ctx2, g') -> case match n1 g' of
              (Just ctx1, g'') -> mergeCtxs ctx1 ctx2 & g''
              _ -> g'
        _ -> g
    where mergeCtxs (is1, n1, nv1, os1) (is2, n2, nv2, os2) =
            (is1, n1, nub([n2] ++ nv1 ++ nv2), nub (os1 ++ os2))
