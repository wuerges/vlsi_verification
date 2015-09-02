module BDD where

import Verilog 
import Graph


import Data.Graph.Inductive
import Control.Arrow
import Debug.Trace

-- | The BDD graph
type BDDG = Gr Int Bool

-- | A bdd has a source and a graph
type BDD = (Int, BDDG)

-- | Creates an initial BDD with a 0 and 1 child or a vertex v
initialBDD :: Int -> BDD
initialBDD v = (v, foldr (&) empty [initialCtx, node1, node0])
    where
        initialCtx = ([], v, v, [(True, 1), (False, 0)]) :: Context Int Bool
        node1      = ([], 1, 1, [])                      :: Context Int Bool
        node0      = ([], 0, 0, [])                      :: Context Int Bool


-- | Creates a BDD for the given vertex
-- | If the vertex is a source, the BDD is simple
-- | if the vertex has many sorces, it must jois all the sources
createBDD :: G -> Int -> BDD
createBDD g n = case pre g n of 
    [] -> initialBDD n
    ps -> foldl1 bddAnd brothers
        where brothers = map (createBDD g) ps

bddAnd :: BDD -> BDD -> BDD
bddAnd (n1, g1) (n2, g2) | n1 > n2  = bddAnd (n2, g2) (n1, g1)
                         | n1 == n2 = undefined

