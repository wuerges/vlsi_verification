module Equivalence where

import Graph
import Data.Graph.Inductive
import BDD
import Verilog

import Data.Maybe


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
equivCompleteBDD g1 g2 is1 is2 = Just $ map (createBDD rg1) is1 == map (createBDD rg2) is2
  where rg1 = g1 `union` empty
        rg2 = g2 `union` empty

