{-# LANGUAGE TemplateHaskell #-}
module QC_Cuts (runTests) where

import Test.QuickCheck
import Data.Graph.Inductive
import Data.Graph.Inductive.Arbitrary()
--import Debug.Trace

import qualified Data.IntMap as I

import BDDGraphCommon
import QC_Graph (TestGraph(TG))
import Cuts

data CutLevel = C G (I.IntMap Node) [Node]
  deriving Show

instance Arbitrary CutLevel where
  arbitrary = do TG x <- arbitrary
                 cs <- sublistOf $ nodes x
                 return $ C x (cutLevelsGraph x cs) cs


prop_cutLevels_all_nodes :: CutLevel -> Bool
prop_cutLevels_all_nodes (C g cls _) = I.size cls  == order g

prop_cutLevels_min_max :: CutLevel -> Bool
prop_cutLevels_min_max (C _ cls cs) =
  all (\(_,v) -> v >= 0 && v <= length cs) $ I.toList cls

return []
runTests = do
  $quickCheckAll
