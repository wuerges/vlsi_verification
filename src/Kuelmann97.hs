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
equivKuelmann97 g1 g2 os1 os2 = checkExits result os1 os2
    where g = g1 `union` g2
          --(_, _, result) = until (checkStop os1 os2) kuelmannStep (M.empty, is0, g)
          (_, _, result) = until (checkStop os1 os2) kuelmannStep (m0, is0, g)
          --m0 = initialMap g is0
          m0 = M.empty

          is0 = bfsn (inputs g) g
          --is0 = topsort g

initialMap :: RG -> [Int] -> M.Map BDD Int
initialMap g is = foldl (\m i -> M.insert (createBDD g i) i m) M.empty is


-- | Checks if both outputs are mapped to the same node in rg
checkExits :: RG -> [Int] -> [Int] -> Bool
checkExits rg os1 os2 = --traceShow (os1, os2) $
  all (\(o1, o2) -> success $ checkPair rg o1 o2) (zip os1 os2)

-- | Checks if a pair in the exit is equivalent or not
checkPair rg o1 o2 = --trace ("o1: " ++ show o1 ++ " o2:" ++ show o2 ++ " result: " ++ show  result) $
  case result  of
    [only] -> Just True
    _      -> Nothing
  where result = filter (\(n, v) -> elem o1 v && elem o2 v ) (labNodes rg)

-- | Checks if analysis should stop
checkStop :: [Int] -> [Int] -> (M.Map BDD Int, [Int], RG) -> Bool
checkStop _ _ (_, [], _) = True
checkStop os1 os2 (_, _, g)  = checkExits g os1 os2

-- | Performs one step of the iteration
kuelmannStep :: (M.Map BDD Int, [Int], RG) -> (M.Map BDD Int, [Int], RG)
kuelmannStep (m, [], g) = (m, [] , g)
kuelmannStep (m, (i:is), g) = trace ("is: " ++ show (bddSize bdd) ++  " -> "++ show (length is) ++ " -> " ++ show (take 5 is) ) (m, is , g)
{-kuelmannStep (m, (i:is), g) | gelem i g = --trace (" WORK LIST: " ++ show (i:is)) $
                                (m', is, g')
                            | otherwise      = (m, is, g)
                            -}
  where
        bdd = createBDD g i
        {-(m', g') = case M.lookup bdd m of
                          Just c -> (m, if c == i then g else mergeNodes g i c)
                          Nothing -> (M.insert bdd i m, g)
        --g' = maybe g (\c -> if c == i then g else mergeNodes g i c) (M.lookup bdd m)
        --is' = nub $ is ++ suc g i
        --m' = M.insertWith (\_ o -> o) bdd i m
        -}
-- | Removes vertices that won't reach any output from the Graph
removeUnreach :: [Int] -> RG -> RG
removeUnreach os g = g -- subgraph (dfs os $ grev g) g

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessors are moved to the first one.
mergeNodes :: RG -> Int -> Int -> RG
mergeNodes g n1 n2 = case match n2 g of
        (Just ctx2, g') -> case match n1 g' of
              (Just ctx1, g'') -> mergeCtxs ctx1 ctx2 & g''
              _ -> g'
        _ -> g
    where mergeCtxs (is1, _, nv1, os1) (_, _, nv2, os2) = --c3 --trace (unlines $ map (("// " ++) . show) [c1, c2, c3]) $ c3
              (is1, n1, nub([n2] ++ nv1 ++ nv2), nub (os1 ++ os2))
            --where c3 =
