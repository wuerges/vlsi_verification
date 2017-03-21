{-# LANGUAGE TemplateHaskell #-}
module QC_Graph  where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary
import Debug.Trace

import BDDGraphCommon
import Graph
import Kuelmann97
import Util

newtype TestGraph = TG G
  deriving Show

removeCycles :: G -> G
removeCycles g = mkGraph ns es
  where
    ns = filter (\(n, _) -> n >= 0) $ labNodes g
    es = filter (\(a, b,_) -> b > a) $ labEdges g

cleanValEdges :: G -> G
cleanValEdges g =
  delEdges [(a,b) | (a,b,_) <- inn g 0] $
    delEdges [(a,b) | (a,b,_) <- inn g 1] g

insZeroAndOne :: G -> G
insZeroAndOne g = g1
  where g0 = if gelem 0 g
                then g
                else insNode (0,"0") g
        g1 = if gelem 1 g0
                then g0
                else insNode (1,"1") g0

instance Arbitrary TestGraph where
  arbitrary =
    do NME x <- arbitrary
       return $ TG $
         removeCycles $
           cleanValEdges $
             insZeroAndOne x

--prop_equiv (TG g) = fst (equivG g g)

prop_node_range1 :: TestGraph -> Bool
prop_node_range1 (TG g) =
  isEmpty g || b >= a
  where
    (a, b) = nodeRange g

prop_node_range2 :: TestGraph -> Bool
prop_node_range2 (TG g) =
  isEmpty g || noConflicts
  where
    noConflicts = all (not . flip gelem g) renumbered
    off = offset g g
    renumbered = map (rnNode off) regular
    regular = filter notVal (nodes g)

prop_node_range3 :: TestGraph -> TestGraph -> Bool
prop_node_range3 (TG g1) (TG g2) =
  isEmpty g1 || isEmpty g2 || noConflicts
  where
    noConflicts = all (not . flip gelem g1) regular
    g2' = renumberGraph (offset g1 g2) g2
    regular = filter notVal (nodes g2')

prop_renumber0 :: TestGraph -> Bool
prop_renumber0 (TG g) = t1 && t2
  where
    t1 = rmdups (labEdges g) == rmdups (labEdges r)
    t2 = rmdups (labNodes g) == rmdups (labNodes r)
    r = renumberGraph 0 g

prop_join_graphs :: TestGraph -> Bool
prop_join_graphs (TG g) =
  order g' * 2 == order gu + length (getInputs g)
  where gu = joinGraphs g g
        g' = (fixSingleNodes g)

prop_self_equiv :: TestGraph -> Bool
prop_self_equiv (TG g) = r
  where gu = joinGraphs g g
        --(r, logs) = equivG gu
        red = reduceG gu
        r = checkEquivRed g g red

return []
runTests = do
  $quickCheckAll
