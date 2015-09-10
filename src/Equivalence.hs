module Equivalence where

import Graph
import Data.Graph.Inductive
import BDD
import Verilog
import qualified Data.Set as S

import Data.Maybe
import Debug.Trace

import qualified Data.Map as M


-- | Type required for the checker functions
type Checker = G -> G -> [Int] -> [Int] -> Maybe Bool

-- | Checks the equivalence between 2 verilog circuits
equiv :: Checker -> Verilog String -> Verilog String -> Maybe Bool
equiv f r1 r2 = f (makeGraphV v1) (makeGraphV v2) (_outputs v1) (_outputs v2)
    where i = foldl attIndexV emptyIndex [r1, r2]
          v1 = verilogToInt r1 i
          v2 = verilogToInt r2 i

-- | Stub for a checker. Always returns Nothing
equivG :: Checker
equivG _ _ _ _ = Nothing


-- | Checks Equivalence by calculating BDDs of all outputs.
-- | Run time is probably exponential.
equivCompleteBDD :: Checker
equivCompleteBDD g1 g2 os1 os2 = Just $ map (createBDD rg1) os1 == map (createBDD rg2) os2
  where rg1 = g1 `union` empty
        rg2 = g2 `union` empty



-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97 :: Checker
equivKuelmann97 g1 g2 [o1] [o2] =
    case checkExitPair g o1 o2 of
      Just x -> Just x
      Nothing -> equiv1 g M.empty (initialInputs g) o1 o2
    where g = g1 `union` g2

-- | Checks if both outputs are mapped to the same node in rg
checkExitPair rg o1 o2 = trace ("checkExitPair: " ++ show (o1, o2)) $
  case filter (\(n, v) -> elem o1 v && elem o2 v ) (labNodes rg) of
    [only] -> Just True
    _      -> Nothing

-- | main workhorse of Kuelmann97
equiv1 :: RG -> M.Map BDD [Int] -> [Int] -> Int -> Int -> Maybe Bool
equiv1 g bddmap inputs o1 o2 = trace ("equiv1: " ++ show inputs)  $
  case cutGraph bddmap inputs g of
    Just (g', bddmap', inputs') -> case checkExitPair g' o1 o2 of
                             Just x -> Just x
                             Nothing -> trace  ("equiv1 dentro: " ++ show inputs') $ equiv1 g' bddmap' inputs' o1 o2
    Nothing -> Nothing



-- | cuts the inputs of a graph
cutGraph :: M.Map BDD [Int] -> [Int] -> RG -> Maybe (RG, M.Map BDD [Int], [Int])
cutGraph bddmap inputs g = trace ("cutGraph: " ++ show inputs)  $
  case filter (\b -> length b > 1) (M.elems bdds) of
    [] -> Just(g, bddmap', inputs'')
      where inputs'' = nextFrontier inputs g g
    merged -> Just (g', bddmap', inputs')
      where g' = foldl mergeNodes g merged
            inputs' = nextFrontier inputs g g'
  where bdds = M.unionWith (++) bddmap (M.fromListWith (++) [(createBDD g i, [i]) | i <- inputs])
        bddmap' = M.map (\(v:_) -> [v]) bdds


nextFrontier :: [Int] -> RG -> RG -> [Int]
nextFrontier ns g g' = S.toList $ S.fromList $ concat $ map (suc g) (filter (flip memberGraph g') ns)


initialInputs :: RG -> [Int]
initialInputs g = [n | n <- nodes g, indeg g n == 0]

mergeNodes :: RG -> [Int] -> RG
mergeNodes g ns = error $ "uninplemented Merging nodes: " ++ show ns
