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
mergeNodes (c1, c2) =
  let (n1, n2) =
        if elem c2 [0,1]
           then (c2, c1)
           else (c1, c2)
        in do
          g <- get
          let es = out g n2 -- getting the edges of n2
              --preparing to add the edges to n1
              es' = [(n1, d, l) | (o, d, l) <- es]
              g' = insEdges es' $ g
          put g'
          purgeNode n1
          purgeNode' n2

purgeNode' :: Node -> KS ()
purgeNode' n = do
  g <- get
  modify $ delNode n
  mapM_ purgeNode [o | (o, _, _) <- inn g n]

purgeNode :: Node -> KS ()
purgeNode n = do
  g <- get
  when (gelem n g && isOutput g n) $ do
    put (delNode n g)
    mapM_ purgeNode [o | (o, _, _) <- inn g n]

runKS :: G -> KS a -> a
runKS g m = r
 where
   r = flip evalState g m


