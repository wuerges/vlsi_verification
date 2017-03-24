module Graph where

import BDDGraphCommon

import Control.Monad
import Control.Monad.State
import Index
import Verilog
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.BFS
import Data.Graph.Inductive.Dot
import Data.Graph.Inductive.NodeMap
import Data.List
import Util
import Control.Arrow
import Debug.Trace
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S
import System.Process
import System.IO

-- import qualified Data.IntMap as M

-- | The graph that models the circuit after Nand Synthesis Model


val :: G -> Node -> String
val g n = maybe "<Node Not Found>" id (lab g n)

type GState a = StateT G IdxState a

dotty s =
  withCreateProcess (proc "dot" ["-Tx11"])
    { std_in = CreatePipe }
    (\ (Just i) b d p -> do hPutStr i $ s
                            hClose i
                            waitForProcess p)

-- | Converts a graph to a GraphViz format
showGraph g = showDot $ fglToDot $ gmap (\(is, n, v, os) -> (is, n, n, os)) g

dottyGraph g = dotty (showGraph g)

-- | Creates all the nodes of the Graph
startGraph :: G
startGraph = mkGraph [(0, show ValZero), (1, show ValOne)] []

addNode :: String -> Node -> GState Int
addNode v n = do
  g <- get
  unless (gelem n g) $
    modify $ insNode (n, v)
  return n


newWire :: GState Int
newWire = do
  n <- lift newIdx
  addNode "<extra>" n

getWire :: Val -> GState Int
getWire w = do
  n <- lift $ getIdx w
  addNode (show w) n


addEdge :: Bool -> (Node, Node) -> GState ()
addEdge b (n1, n2) =
  modify $ insEdge (n1, n2, b)

initGraph :: Verilog -> GState ()
initGraph v = do
  mapM_ getWire (map Input $ _inputs v)

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
  modify $ insEdges [(i,o, False) | o <- os]

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
  n <- newWire
  modify $ insEdge (n, o, False)
  embedAnd' is n

-- | Inserts an Or function into the graph
embedOr :: [Val] -> Val -> GState ()
embedOr iws ow = do
  is <- mapM getWire iws
  o <- getWire ow
  embedOr' is o

embedOr' :: [Node] -> Node -> GState ()
embedOr' is o = do
  n <- newWire
  modify $ insEdge (n, o, False)
  embedNor' is n

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
  n <- newWire
  modify $ insEdge (n, o, False)
  embedXor' is n




-- | Replaces a node with only one input and one output with an edge.
fixSingleNode :: Int -> G -> G
fixSingleNode n g =  case match n g of
    (Nothing, _)  -> error "Could not match context in fixSingleNode"
    (Just ctx, g') -> case ctx of
       ([(vi, ni)], _, _, (o:os)) ->
         insEdges [(ni, no, not $ vi /= vo) | (vo, no) <- (o:os)] g'
       _                               -> g

-- | Cleans up graph after adding extra single nodes
fixSingleNodes g = --trace ("fix singles ")
    g'
  where g' = foldr fixSingleNode g (nodes g)

makeGraphV :: [Verilog] -> G
makeGraphV vs = g
  where
    (g, _, _, _) = makeGraphV' vs

makeGraphV' :: [Verilog] -> (G, M.Map Val Int, M.Map Val Int, M.Map String Int)
makeGraphV' vs =
  runIdx $ flip evalStateT startGraph $ do
    mapM_ initGraph vs
    mapM_ makeGraphV1 vs
    fixSingleNodes <$> get

makeGraphV1 :: Verilog -> GState G
makeGraphV1 v = do
  inputs <- mapM getWire (map Input $ _inputs v)
  wires <- mapM getWire (map Wire $ _inputs v)
  mapM_ (addEdge True) $ zip inputs wires

  mapM embedF $ reverse $ _functions v

  wire_outs <- mapM getWire (map Wire $ _outputs v)
  outs <- mapM getWire (map Output $ _outputs v)
  mapM_ (addEdge True) $ zip wire_outs outs

  lift resetIdx
  get

-- | Calculates the nodes without input edges
isInput g n = indeg g n == 0

getInputs :: G -> [Node]
getInputs g = filter (isInput g) $ nodes g

mybfs :: G -> [Node]
mybfs = topsort
--mybfs = sort . nodes
--mybfs g = bfsn [n | n <- nodes g, indeg g n == 0] g


-- | Calculates the nodes without output edges
isOutput g n = outdeg g n == 0

getOutputs :: G -> [Node]
getOutputs g = filter (isOutput g) $ nodes g



-- | Silly stuff from here
--

renumberGraph :: Int -> G -> G
renumberGraph offset g = mkGraph ns es
  where
    ns = map (rnLabNode offset) (labNodes g)
    es = map (rnLabEdge offset) (labEdges g)

rnLabEdge off (a, b, l) =
  ( rnNode off a
  , rnNode off b
  , l)

rnLabNode off (a, l) = (rnNode off a, l)


notVal :: Int -> Bool
notVal x = (x /= 0) && (x /= 1)

offset :: G -> G -> Int
offset g1 g2 = (max b1 b2 - min a1 a2) + 1
  where (a1, b1) = nodeRange g1
        (a2, b2) = nodeRange g2

rnNode off n = n + off

renumberGraph' g1 g2 = renumberGraph off g2
  where off = offset g1 g2

joinGraphs :: G -> G -> G
joinGraphs g1 g2 =
  fixSingleNodes $ insEdges es' gu
  where
    --off = offset g1 g2
    g2' = renumberGraph' g1 g2

    gu = mkGraph (rmdups $ labNodes g1 ++ labNodes g2') (labEdges g1 ++ labEdges g2')

    is1 = getInputs g1
    is2 = getInputs g2'

    is = zip (sort is1) (sort is2)
    es' = [(a, b, True) | (a,b) <- is]

mergeNodes' :: (Node, Node) -> G -> G
mergeNodes' (n1, n2) g
  | gelem n1 g && gelem n2 g = (is1, n1, v1, os') & g''
  | otherwise = g
  where
    (Just (is1, _, v1, os1), g' ) = match n1 g
    (Just (_  , _, _ , os2), g'') = match n2 g'
    os' = S.toList $ S.delete (False, n1) $ S.delete (True, n1) $ S.fromList $ os1 ++ os2


