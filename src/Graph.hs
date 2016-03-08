module Graph where

import Verilog
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.List
import Control.Arrow
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Map as M

-- | The graph that models the circuit after Nand Synthesis Model
type G = Gr () Bool
type RG = Gr [Int] Bool

-- | Converts a graph to a GraphViz format
showGraph g = showDot $ fglToDot $ gmap (\(is, n, _, os) -> (is, n, show n, os)) g

-- | Creates all the nodes of the Graph
wireNodes :: Verilog Int -> [Context () Bool]
wireNodes v = [([], n, (), []) | n <- names v]

-- | Embeds all the wires in the graphs as disconnected nodes
embedWires :: Verilog Int -> G -> G
embedWires v g = foldr (&) g (wireNodes v)

trues  = repeat True
falses = repeat False

-- | Embeds a Function in the graph.
embedF :: Function Int -> G -> G
embedF (Fun op os is) g =
    case op of
    --case trace ("// embedF -> " ++ show (op, os, is) ++ "\n" ++  showGraph g)  op of
       And  -> embedAnd is o g
       Nand -> embedNand is o g
       Or   -> embedOr is o g
       Nor  -> embedNor is o g
       Xor  -> embedXor is o g
       Xnor -> embedXnor is o g
       Buf  -> embedBuf i os g
       Not  -> embedNot i os g
    where [i] = is
          [o] = os

-- | Negates an Adjacency list
negateA :: Adj Bool -> Adj Bool
negateA = map (first not)

-- | Negates all output edges of a ctx
negateCtx (is, n, nv, os) = (is, n, nv, negateA os)

-- | Negates all output edges of a node
negateV :: Int -> G -> G
negateV n g = let (mc, g') = match n g
              in case mc of
                Just ctx -> negateCtx ctx & g'
                Nothing -> error "could not find vertex in negateV"

-- | Should insert a few edges in the graph.
embedBuf :: Int -> [Int] -> G -> G
embedBuf i os g = foldr (\o g -> insEdge (i, o, True) g) g os

-- | Inserts a Not function in the graph.
-- | Does this by negating all the outputs of a current vertex.
embedNot i os g = foldr (\o g -> insEdge (i, o, True) g) (foldr negateV g os) os

-- | Inserts an And function into the graph
embedAnd :: [Int] -> Int -> G -> G
embedAnd is o g = foldr (\i g -> insEdge (i, o, True) g) g is

-- | Inserts a Nand function into the graph
embedNand :: [Int] -> Int -> G -> G
embedNand is o g = embedAnd is o (negateV o g)

-- | Inserts an Or function into the graph
embedOr :: [Int] -> Int -> G -> G
embedOr is o g = embedNor is o (negateV o g)


-- | Inserts a Nor function into the graph
embedNor :: [Int] -> Int -> G -> G
embedNor is o g = foldr (\i g -> insEdge (i, o, False) g) g is

-- | Inserts a Xor function into the graph
embedXor :: [Int] -> Int -> G -> G
embedXor [i1, i2] o g = g''
    where [n1, n2] = newNodes 2 g
          g'  = insNodes [(n1, ()), (n2, ())] g
          g'' = embedOr [i1, i2] n2
              $ embedNand [i1, i2] n1
              $ embedAnd [n1, n2] o g'
embedXor (i1:i2:is) o g = g'
    where [n]  = newNodes 1 g
          g'   = embedXor (n:is) o $ embedXor [i1, i2] n $ insNode (n, ()) g

-- | Inserts a Xnor gate into the graph
embedXnor is o g = embedXor is o (negateV o g)

-- | Replaces a node with only one input and one output with an edge.
fixSingleNode :: Int -> G -> G
fixSingleNode n g =  case match n g of
    (Just ctx, g') -> case ctx of
       ([(vi, ni)], _, _, [(vo, no)]) -> insEdge (ni, no, not $ vi /= vo) g'
       _                               -> g
    (Nothing, _)  -> error "Could not match context in fixSingleNode"

-- | Cleans up graph after adding extra single nodes
fixSingleNodes g = --trace ("// fix singles \n" ++ showGraph g ++ "\n//fixed:\n" ++ showGraph g') g'
    g'
  where g' = foldr fixSingleNode g (nodes g)

makeGraphV v = fixSingleNodes $ foldr embedF (embedWires v empty) (reverse $ _functions v)

{-
-- | Checks if a node is an output
outut :: Gr a b -> Int -> Bool
outut g n = outdeg g n == 0

-- | Checks if a node is an input
input :: Gr a b -> Int -> Bool
input g n = indeg g n == 0
-}

-- | Calculates the nodes without input edges
inputs :: Gr a b -> [Int]
inputs g = [n | n <- nodes g, indeg g n == 0]

-- | Calculates the nodes without output edges
outputs :: Gr a b -> [Int]
outputs g = [n | n <- nodes g, outdeg g n == 0]


-- | Renumber the nodes according solely to their inputs,
-- | so nodes with the same inputs will have the same id
-- | regardless of the previous.
--renameNodes :: G -> Int -> RG
--renameNodes g i = gmap renameCtxNode g
    --where renameCtxNode ([], n, (), os)  = ([], n, [n], os)
          --renameCtxNode (is, n, (), os)  = (is, n+i, [n,n+i], os)



-- | Joins 2 graphs into one, merging the nodes with the same inputs.
union :: G -> G -> RG
union g1 g2 = g'
  --trace ("\n// union 3: \n" ++ showGraph g1 ++ "\n" ++  showGraph g2 ++ "\n" ++  showGraph g') g' --traceShow g'' g'
    where
          g' = mkGraph (n_g1 ++ n_g2) (e_g1 ++ e_g2)

          n_g1 = map (renameNode 0) $ labNodes g1
          e_g1 = map (renameEdge 0) $ labEdges g1

          n_g2 = map (renameNode (mn + 10)) $ labNodes g2
          e_g2 = map (renameEdge (mn + 10)) $ labEdges g2

          (_, mn) = nodeRange g1

          renameNode :: Int -> LNode () -> LNode [Int]
          renameNode i (n, ()) = (rn i n, [n])

          renameEdge :: Int -> LEdge Bool -> LEdge Bool
          renameEdge i (f, t, v) = (rn i f, rn i t, v)

          rn i n | elem n (inputs g2) = n
                 | otherwise          = i + n


mybfs :: Gr a b -> [Int]
mybfs g | isEmpty g = []
        | otherwise = inputs g ++ (mybfs $ delNodes (inputs g) g)


