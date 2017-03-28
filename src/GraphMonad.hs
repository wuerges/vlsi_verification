{-# LANGUAGE FlexibleContexts #-}
module GraphMonad where

import BDDGraphCommon
import Graph
import Control.Monad.State
import Data.Graph.Inductive
--import Debug.Trace

modifyG :: (G -> G) -> KS ()
modifyG f = modify $ \s -> s { graph = f (graph s) }

putG :: G -> KS ()
putG g = modifyG $ const g

-- | Merges 2 nodes in the graph.
-- | The left one is removed and the right one is mantained
-- | All the sucessors are moved to the node that remains.
-- | returns the remaining node

mergeNodes :: Node -> Node -> KS ()
mergeNodes n1 n2 = do
  when (n1 /= n2) $
    modifyG $ mergeNodes' (min n1 n2, max n1 n2)
  --when (n2 /= 0 && n2 /= 1) $ addCut n2


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

