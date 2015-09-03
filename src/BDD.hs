module BDD where

import Verilog
import Graph

import Data.Graph.Inductive
import Control.Arrow
import Debug.Trace

-- | A bdd has a source and a graph
data BDD = Zero | One | B BDD Int BDD
  deriving Show

-- | Creates an initial BDD with a 0 and 1 child for a variable v
initialBDD :: Int -> BDD
initialBDD v = B Zero v One

-- | Creates a BDD for the given vertex
-- | If the vertex is a source, the BDD is simple
-- | if the vertex has many sorces, it must join all the sources
createBDD :: G -> Int -> BDD
createBDD g n = case lpre g n of
    [] -> initialBDD n
    ps -> foldl1 bddAnd (map createBDDe ps)
        where createBDDe (p, b) | b == True  = createBDD g p
                                | b == False = negateBDD $ createBDD g p

-- | Negates the BDD (inverts Zeros and Ones)
negateBDD Zero      = One
negateBDD One       = Zero
negateBDD (B z v o) = B (negateBDD z) v (negateBDD o)


-- | Joist 2 BDDs by a conjunction (and function)
bddAnd :: BDD -> BDD -> BDD

bddAnd Zero _ = Zero
bddAnd _ Zero = Zero
bddAnd One  b = b
bddAnd b  One = b

bddAnd b1@(B z1 v1 o1) b2@(B z2 v2 o2) | v1 > v2 = B (bddAnd z1 b2) v1 (bddAnd o1 b2)
                                       | v1 == v2 = B (bddAnd z1 z2) v1 (bddAnd o1 o2)
                                       | v1 < v2 = B (bddAnd z2 b1) v2 (bddAnd o2 b1)

