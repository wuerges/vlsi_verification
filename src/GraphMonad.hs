{-# LANGUAGE FlexibleContexts #-}
module GraphMonad where

import Graph
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

import qualified Data.Set as S
import Text.Printf

import BDDGraphMonad

modifyG :: (G -> G) -> KS ()
modifyG f = modify $ \s -> s { graph = f (graph s) }

putG :: G -> KS ()
putG g = modifyG $ const g

-- | Merges 2 nodes in the graph.
-- | The left one is removed and the right one is mantained
-- | All the sucessors are moved to the node that remains.
-- | returns the remaining node

mergeNodesT :: Node -> Node -> KS ()
mergeNodesT n1 n2 = do
  mergeNodes (n1, n2)
  when (n2 /= 0 && n2 /= 1) $ addCut n2

mergeNodes :: (Node, Node) -> KS ()
mergeNodes (n1, n2) = do
    g <- getG
    let es = out g n2 -- getting the edges of n2
        --preparing to add the edges to n1
        es' = [(n1, d, l) | (o, d, l) <- es]
        g' = insEdges es' $ g
    putG $ delEdge (n1, n1) g'
    --purgeNode n1
    purgeNode' n2

purgeNode' :: Node -> KS ()
purgeNode' n = do
  g <- getG
  modifyG $ delNode n
  mapM_ purgeNode [o | (o, _, _) <- inn g n]

purgeNode :: Node -> KS ()
purgeNode n = do
  g <- getG
  when (n /= 0 && n /= 1 && gelem n g && isOutput g n && not (isInput g n)) $ do
    putG (delNode n g)
    mapM_ purgeNode [o | (o, _, _) <- inn g n]


addCut :: Node -> KS ()
addCut n = do
  s <- get
  put $ s { cuts = n:(cuts s) }

