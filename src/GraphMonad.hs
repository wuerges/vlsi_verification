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

type KS = State G

-- | Merges 2 nodes in the graph.
-- | The left one is removed and the right one is mantained
-- | All the sucessors are moved to the node that remains.
-- | returns the remaining node
mergeNodes :: (Node, Node) -> KS ()
mergeNodes (n1, n2) = do
    g <- get
    let es = out g n2 -- getting the edges of n2
        --preparing to add the edges to n1
        es' = [(n1, d, l) | (o, d, l) <- es]
        g' = insEdges es' $ g
    put $ delEdge (n1, n1) g'
    --purgeNode n1
    purgeNode' n2

purgeNode' :: Node -> KS ()
purgeNode' n = do
  g <- get
  modify $ delNode n
  mapM_ purgeNode [o | (o, _, _) <- inn g n]

purgeNode :: Node -> KS ()
purgeNode n = do
  g <- get
  when (n /= 0 && n /= 1 && gelem n g && isOutput g n && not (isInput g n)) $ do
    put (delNode n g)
    mapM_ purgeNode [o | (o, _, _) <- inn g n]

runKS :: G -> KS a -> a
runKS g m = r
 where
   r = flip evalState g m


