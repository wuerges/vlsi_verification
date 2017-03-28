module Equivalence where

--import Graph
--import Data.Graph.Inductive
import Verilog
--import qualified Data.Set as S
--import Control.Arrow

--import Data.Maybe
--import Debug.Trace

--import qualified Data.Map as M


-- | Type required for the checker functions
type Checker = Verilog -> Verilog -> Either String Bool

-- | Stub for a checker. Always returns False
--equiv :: Checker
--equiv _ _ = Left "Dummy checker"


-- | Checks Equivalence by calculating BDDs of all outputs.
-- | Run time is probably exponential.
{-equivCompleteBDD :: Checker
equivCompleteBDD g1 g2 os1 os2 = map (createBDD rg1) os1 == map (createBDD rg2) os2
  where rg1 = g1 `union` empty
        rg2 = g2 `union` empty

-}
