module Graph where

import Control.Monad
import Verilog
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.NodeMap
import Data.List
import Control.Arrow
import Debug.Trace
import System.Random
import qualified Data.Set as S
import qualified Data.IntMap as M

-- | The graph that models the circuit after Nand Synthesis Model

data NT = Wire String | Zero | One

type G = Gr NT Bool
type Ctx = Context NT Bool

type VG = Gr (NT, Bool) Bool

-- | Converts a graph to a GraphViz format
showGraph g = showDot $ fglToDot $ gmap (\(is, n, v, os) -> (is, n, show n, os)) g

-- | Creates all the nodes of the Graph
--wireNodes :: Verilog -> Ctx
--wireNodes v = [([], n, (), []) | n <- names v]

-- | Embeds all the wires in the graphs as disconnected nodes
initGraph :: Verilog -> G -> G
initGraph v g = g'
  where m = fromGraph g
        g' = insMapNodes_ m (Zero:One:named) g
        named = map Wire $ names v

trues  = repeat True
falses = repeat False

-- | Embeds a Function in the graph.
embedF :: Function -> G -> G
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
negateV :: String -> G -> G
negateV name g =
  let m = fromGraph g
      (n, _) = mkNode_ m $ Wire name
      (mc, g') = match n g
   in case mc of
        Just ctx -> negateCtx ctx & g'
        Nothing -> error "could not find vertex in negateV"

-- | Should insert a few edges in the graph.
embedBuf :: String -> [String] -> G -> G
embedBuf i os g =
  run_ g $ insMapEdgesM [(Wire i, Wire o, True) | o <- os]

-- | Inserts a Not function in the graph.
-- | Does this by negating all the outputs of a current vertex.
embedNot :: String -> [String] -> G -> G
embedNot i os g =
  embedBuf i os $ foldr negateV g os

-- | Inserts an And function into the graph
embedAnd :: [String] -> String -> G -> G
embedAnd is o g =
  run_ g $ insMapEdgesM [(Wire i, Wire o, True) | i <- is]

-- | Inserts a Nand function into the graph
embedNand :: [String] -> String -> G -> G
embedNand is o g =
  embedAnd is o $ negateV o g

-- | Inserts an Or function into the graph
embedOr :: [String] -> String -> G -> G
embedOr is o g =
  embedNor is o $ negateV o g

-- | Inserts a Nor function into the graph
embedNor :: [String] -> String -> G -> G
embedNor is o g =
  run_ g $ insMapEdgesM [(Wire i, Wire o, False) | i <- is]



-- | Inserts a Xor function into the graph
embedXor :: [String] -> String -> G -> G
embedXor [i1, i2] o g =
  embedOr [i1, i2] n2 $
    embedNand [i1, i2] n1 $
      embedAnd [n1, n2] o $
        run_ g $
          insMapNodeM aux1
          insMapNodeM aux2

  where
    aux1 = i1 ++ "_extra_wire"
    aux2 = i2 ++ "_extra_wire"

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

makeGraphV v = fixSingleNodes $ foldr embedF (initGraph v empty) (reverse $ _functions v)

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

-- | Joins 2 graphs into one, merging the nodes with the same inputs.
union :: G -> G -> (G, [Node], [Node])
union g1 g2 = (g', outputs g1, map (+(mn + 10)) (outputs g2))
  --trace ("\n// union 3: \n" ++ showGraph g1 ++ "\n" ++  showGraph g2 ++ "\n" ++  showGraph g') g' --traceShow g'' g'
    where
          g' = mkGraph (n_g1 ++ n_g2) (e_g1 ++ e_g2)

          n_g1 = map (renameNode 0) $ labNodes g1
          e_g1 = map (renameEdge 0) $ labEdges g1

          n_g2 = map (renameNode (mn + 10)) $ labNodes g2
          e_g2 = map (renameEdge (mn + 10)) $ labEdges g2

          (_, mn) = nodeRange g1

          renameNode :: Int -> LNode () -> LNode ()
          renameNode i (n, ()) = (rn i n, ())

          renameEdge :: Int -> LEdge Bool -> LEdge Bool
          renameEdge i (f, t, v) = (rn i f, rn i t, v)

          rn i n | elem n (inputs g2) = n
                 | otherwise          = i + n




mybfs :: Gr a b -> [Int]
mybfs g | isEmpty g = []
        | otherwise = inputs g ++ (mybfs $ delNodes (inputs g) g)



-- | simulates the circuit's behavior.
-- | Receives the graph of the circuit as input and a list of inputs, in order.
-- | produces the outputs, in order.

simulate1 :: Context () Bool -> M.IntMap Bool -> M.IntMap Bool
simulate1 ([], n, (), _) m = case M.lookup n m of
                                Just v ->  m
                                Nothing -> error "Value for n should have been set."
simulate1 (is, n, (), _) m = M.insert n v m
  where
    v = and $ [m M.! i /= vi | (vi, i) <- is]


simulate :: [(Int, Bool)] -> G -> [(Int, Bool)]
simulate input_values g = [(o, m' M.! o) | o <- outputs g]
  where
    m  = M.fromList input_values
    m' = ufold simulate1 m g

randomSimulateIO :: G -> IO [(Int, Bool)]
randomSimulateIO g = do
    let is = inputs g
    rs <- replicateM (length is) randomIO
    return $ simulate (zip is rs) g

contexts :: G -> [Ctx]
contexts g = map (context g) (topsort g) --ufold (:) [] g


removeStuckAt0 :: [Int] -> G -> G
removeStuckAt0 = undefined -- TODO
