module Graph where

import Control.Monad
import Control.Monad.State
import Index
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

type G = Gr Val Bool
type Ctx = Context Val Bool


val :: G -> Node -> Val
val g n = v
  where
    Just v = lab g n

type GState a = StateT G IdxState a

--type VG = Gr (NT, Bool) Bool

-- | Converts a graph to a GraphViz format
showGraph g = showDot $ fglToDot $ gmap (\(is, n, v, os) -> (is, n, (n, v), os)) g

-- | Creates all the nodes of the Graph
--wireNodes :: Verilog -> Ctx
--wireNodes v = [([], n, (), []) | n <- names v]

-- | Embeds all the wires in the graphs as disconnected nodes

startGraph :: G
startGraph = mkGraph [(0, ValZero), (1, ValOne)] []

addNode :: (Val, Node) -> GState Int
addNode (v, n) = do
  g <- get
  unless (gelem n g) $
    modify $ insNode (n,v)
  return n


newWire :: GState Int
newWire = do
  n <- lift newIdx
  addNode (Wire "<extra>", n)

getWire :: Val -> GState Int
getWire w = do
  n <- lift $ getIdx w
  addNode (w, n)


initGraph :: Verilog -> GState ()
initGraph v = do
  ns <- lift $ getInputs $ _inputs v
  mapM_ addNode $ zip (map Wire $ _inputs v) ns

trues  = repeat True
falses = repeat False

-- | Embeds a Function in the graph.
embedF :: Function -> GState ()
embedF f@(Fun op os is) =
  --trace ("Embedding function" ++ show f) $
    case op of
      And  -> embedAnd is o
      Nand -> embedNand is o
      Or   -> embedOr is o
      Nor  -> embedNor is o
      Xor  -> embedXor is o
      Xnor -> embedXnor is o
      Buf  -> embedBuf i os
      Not  -> embedNot i os
    where [i] = is
          [o] = os

-- | Negates an Adjacency list
negateA :: Adj Bool -> Adj Bool
negateA = map (first not)

-- | Negates all output edges of a ctx
negateCtx (is, n, nv, os) = (is, n, nv, negateA os)


-- | Negates all output edges of a node
negateV :: Node -> GState ()
negateV n = do
  g <-  get
  let (mc, g') = match n g
  case mc of
    Just ctx -> put $ negateCtx ctx & g'
    Nothing -> error "could not find vertex in negateV"

-- | Should insert a few edges in the graph.
embedBuf :: Val -> [Val] -> GState ()
embedBuf iw ows = do
    i <- getWire iw
    os <- mapM getWire ows
    embedBuf' i os

embedBuf' :: Node -> [Node] -> GState ()
embedBuf' i os = do
    modify $ insEdges [(i,o, True) | o <- os]

-- | Inserts a Not function in the graph.
-- | Does this by negating all the outputs of a current vertex.
embedNot :: Val -> [Val] -> GState ()
embedNot iw ows = do
  i <- getWire iw
  os <- mapM getWire ows
  mapM_ negateV os
  embedBuf' i os

-- | Inserts an And function into the graph
embedAnd :: [Val] -> Val -> GState ()
embedAnd iws ow = do
    is <- mapM getWire iws
    o <- getWire ow
    embedAnd' is o

embedAnd' :: [Node] -> Node -> GState ()
embedAnd' is o =
  modify $ insEdges [(i,o, True) | i <- is]

-- | Inserts a Nand function into the graph
embedNand :: [Val] -> Val -> GState ()
embedNand iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  embedNand' is o

embedNand' :: [Node] -> Node -> GState ()
embedNand' is o = do
  negateV o
  embedAnd' is o

-- | Inserts an Or function into the graph
embedOr :: [Val] -> Val -> GState ()
embedOr iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  embedOr' is o

embedOr' :: [Node] -> Node -> GState ()
embedOr' is o = do
  negateV o
  embedNor' is o

-- | Inserts a Nor function into the graph
embedNor :: [Val] -> Val -> GState ()
embedNor iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  embedNor' is o


embedNor' :: [Node] -> Node -> GState ()
embedNor' is o =
  modify $ insEdges [(i, o, False) | i <- is]



-- | Inserts a Xor function into the graph
embedXor :: [Val] -> Val -> GState ()
embedXor iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  embedXor' is o

embedXor' :: [Node] -> Node -> GState ()
embedXor' [i1, i2] o = do
  n1 <- newWire
  n2 <- newWire
  embedAnd' [n1, n2] o
  embedNand' [i1, i2] n1
  embedOr' [i1, i2] n2

embedXor' (i1:i2:is) o = do
  n <- newWire
  embedXor' (n:is) o
  embedXor' [i1, i2] n

-- | Inserts a Xnor gate into the graph
embedXnor iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  negateV o
  embedXor' is o


-- | Replaces a node with only one input and one output with an edge.
fixSingleNode :: Int -> G -> G
fixSingleNode n g =  case match n g of
    (Just ctx, g') -> case ctx of
       ([(vi, ni)], _, _, [(vo, no)]) -> insEdge (ni, no, not $ vi /= vo) g'
       _                               -> g
    (Nothing, _)  -> error "Could not match context in fixSingleNode"

-- | Cleans up graph after adding extra single nodes
fixSingleNodes g = trace ("fix singles ")
    g'
  where g' = foldr fixSingleNode g (nodes g)


makeGraphV :: [Verilog] -> G
makeGraphV vs =
  runIdx $ flip evalStateT startGraph $ do
    mapM_ initGraph vs
    mapM_ makeGraphV1 vs
    get


makeGraphV1 :: Verilog -> GState G
makeGraphV1 v = do
  mapM embedF $ reverse $ _functions v
  lift resetIdx
  get
--fixSingleNodes $
  --trace "Finished Embeddeding all functinos"  $

-- | Calculates the nodes without input edges
inputs :: Gr a b -> [Int]
inputs g = [n | n <- nodes g, indeg g n == 0]

-- | Calculates the nodes without output edges
outputs :: Gr a b -> [Int]
outputs g = [n | n <- nodes g, outdeg g n == 0]

-- | Renumber the nodes according solely to their inputs,
-- | so nodes with the same inputs will have the same id
-- | regardless of the previous.


mybfs :: Gr a b -> [Int]
mybfs g | isEmpty g = []
        | otherwise = inputs g ++ (mybfs $ delNodes (inputs g) g)



-- | simulates the circuit's behavior.
-- | Receives the graph of the circuit as input and a list of inputs, in order.
-- | produces the outputs, in order.

simulate1 :: Context () Bool -> M.IntMap Bool -> M.IntMap Bool
simulate1 = undefined
{-
simulate1 ([], n, (), _) m = case M.lookup n m of
                                Just v ->  m
                                Nothing -> error "Value for n should have been set."
simulate1 (is, n, (), _) m = M.insert n v m
  where
    v = and $ [m M.! i /= vi | (vi, i) <- is]
-}

simulate :: [(Int, Bool)] -> G -> [(Int, Bool)]
simulate = undefined
  {-
simulate input_values g = [(o, m' M.! o) | o <- outputs g]
  where
    m  = M.fromList input_values
    m' = ufold simulate1 m g
    -}

randomSimulateIO :: G -> IO [(Int, Bool)]
randomSimulateIO g = do
    let is = inputs g
    rs <- replicateM (length is) randomIO
    return $ simulate (zip is rs) g

contexts :: G -> [Ctx]
contexts g = map (context g) (topsort g) --ufold (:) [] g


removeStuckAt0 :: [Int] -> G -> G
removeStuckAt0 = undefined -- TODO
