module BDD () where

import Verilog
import Graph

import Control.Parallel
import Data.Graph.Inductive
import Control.Arrow
import Debug.Trace
import Data.Maybe
import qualified Data.IntMap as M
import Control.Monad.State

-- | A bdd has a source and a graph
data BDD = Zero | One | B BDD Int BDD
  deriving (Eq, Ord, Show)

instance Monoid BDD where
  mempty = One
  mappend = bddAnd

-- | Creates an initial BDD with a 0 and 1 child for a variable v
initialBDD :: Int -> BDD
initialBDD v = B Zero v One

-- | Negates the BDD (inverts Zeros and Ones)
negateBDD Zero      = One
negateBDD One       = Zero
negateBDD (B z v o) = B (negateBDD z) v (negateBDD o)


-- | Joint 2 BDDs by a conjunction (and function)
bddAnd :: BDD -> BDD -> BDD

bddAnd Zero _ = Zero
bddAnd _ Zero = Zero
bddAnd One  b = b
bddAnd b  One = b

bddAnd b1@(B z1 v1 o1) b2@(B z2 v2 o2) | v1 > v2 = B (bddReduce $ bddAnd z1 b2) v1 (bddReduce $ bddAnd o1 b2)
                                       | v1 == v2 = B (bddReduce $ bddAnd z1 z2) v1 (bddReduce $ bddAnd o1 o2)
                                       | v1 < v2 = B (bddReduce $ bddAnd z2 b1) v2 (bddReduce $ bddAnd o2 b1)

-- | Reduces a OBDD into a ORBDD
bddReduce :: BDD -> BDD
bddReduce Zero = Zero
bddReduce One = One
bddReduce b@(B z v o) | bddReduce z == bddReduce o    = z
                      | otherwise                    = b

-- | Calculates the size of a BDD
bddSize :: BDD -> Int
bddSize Zero = 1
bddSize One = 1
bddSize (B z _ o) = bddSize z + 1 + bddSize o

