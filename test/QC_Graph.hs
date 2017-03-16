{-# LANGUAGE TemplateHaskell #-}
module QC_Graph  where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary
import Debug.Trace

import Graph
import Kuelmann97

newtype TestGraph = TG G
  deriving Show

instance Arbitrary TestGraph where
  arbitrary = do NL (NME x) <- arbitrary
                 return $ TG x

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
prop_renumber0 (TG g) =
  renumberGraph 0 g == g

prop_self_equiv :: TestGraph -> Bool
prop_self_equiv (TG g) =
  trace ("TRACE: " ++ show gu ++  concat logs) r
  where gu = joinGraphs g g
        (r, logs) = equivG gu

return []
runTests = do
  $quickCheckAll
