module Equivalence where

import Graph
import Data.Graph.Inductive
import BDD
import Verilog
import qualified Data.Set as S

import Data.Maybe
import Debug.Trace

import qualified Data.Map as M


type EqRes = Maybe Bool

success :: EqRes -> Bool
success Nothing  = False
success (Just _) = True

-- | Type required for the checker functions
type Checker = G -> G -> [Int] -> [Int] -> Bool

-- | Checks the equivalence between 2 verilog circuits
equiv :: Checker -> Verilog String -> Verilog String -> Bool
equiv f r1 r2 = f (makeGraphV v1) (makeGraphV v2) (_outputs v1) (_outputs v2)
    where i = foldl attIndexV emptyIndex [r1, r2]
          v1 = verilogToInt r1 i
          v2 = verilogToInt r2 i

-- | Stub for a checker. Always returns False
equivG :: Checker
equivG _ _ _ _ = False


-- | Checks Equivalence by calculating BDDs of all outputs.
-- | Run time is probably exponential.
equivCompleteBDD :: Checker
equivCompleteBDD g1 g2 os1 os2 = map (createBDD rg1) os1 == map (createBDD rg2) os2
  where rg1 = g1 `union` empty
        rg2 = g2 `union` empty

