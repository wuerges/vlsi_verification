{-# LANGUAGE TemplateHaskell #-}
module QC_Graph (runTests) where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.Graph.Inductive

import Graph

return []
runTests = do
  $quickCheckAll
