{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Verilog
import Equivalence
import Graph
import BDDGraph (BDDState, runBDDState) --Graph (BDD, initialBDD, negateBDD, bddOne, bddAnd)
import BDD

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Class
import Data.Maybe

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

import qualified Data.Set as S

--type KS a = WriterT String (State (G, M.Map BDD Node)) a
--
--type KS a = WriterT [Log] (State (G, M.Map BDD Node, M.Map Node BDD)) a
type KS = WriterT [String] (StateT (G, M.Map BDD Node, M.Map Node BDD) BDDState)

getNodeBddM :: KS (M.Map Node BDD)
getNodeBddM = do
  (_, _, m) <- get
  return m

getBddNodeM :: KS (M.Map BDD Node)
getBddNodeM = do
  (_, m, _) <- get
  return m

getBDD :: Node -> KS (Maybe BDD)
getBDD n = do
  M.lookup n <$> getNodeBddM

getBDDfromEdge :: LEdge Bool -> KS (Maybe BDD)
getBDDfromEdge (o, _, v) = do
  bdd <- getBDD o
  if v then return bdd
       else return $ negateBDD <$> bdd

putG :: G -> KS ()
putG g = do
  (_, x, y) <- get
  put (g, x, y)

getG :: KS G
getG = do
  (g, _, _) <- get
  return g


-- | Merges 2 nodes in the graph.
-- | The smallest one is mantained, the other one is removed.
-- | All the sucessors are moved to the node that remains.
-- | Return the node that remains.
mergeNodes :: Node -> Node -> KS Node
mergeNodes c1 c2 = do
  (g, m1, m2) <- get
  let [n1, n2] = sort [c1, c2] -- n1 is the smallest.
      es = out g n2 -- getting the edges of n2
      des = [(o, d) | (o, d, l) <- es] -- preparing to remove the edges from n2
      es' = [(n1, d, l) | (o, d, l) <- es] --preparing to add the edgesg to n1
      g' = insEdges es' $ delEdges des g

  put (g', m1, m2)
  purgeNode n2
  g'' <- getG
  --lift $ tell [(g, "before merge of " ++ show (n1,n2))]
  --lift $ tell [(g', "after the merge of " ++ show (n1,n2))]
  --lift $ tell [(g'', "after the purge of " ++ show n2)]
  return $ n1


isWire n g = case l of
               Wire _ -> True
               _ -> False
  where Just l = lab g n


purgeNode :: Node -> KS ()
purgeNode n = do
  g <- getG
  when (gelem n g && isWire n g && outdeg g n == 0) $ do
    putG (delNode n g)
    mapM_ purgeNode [o | (o, _, _) <- inn g n]




getPreds :: Node -> KS (Node, [(Node, Bool)])
getPreds y = do
  g <- getG
  return (y, sort $ [(o, v) | (o, d, v) <- inn g y])

checkResult :: KS (Either String Bool)
checkResult =  do
  (g, _, _) <- get
  ps <- mapM getPreds (getOutputs g)
  let ps'  = sortBy (\a b -> snd a `compare` snd b) ps
      ps'' = groupBy (\a b -> snd a == snd b) ps'
      r = all (\x -> length x >= 2) ps''
  return $ -- $ traceShow (getOutputs g, ps'', r) $
    Right r
    --error $
      --"uninplemented" ++ show [(n, l) | (n, l) <- labNodes g, outdeg g n == 0] ++
        --showGraph g
    --where
  {-do
  case (length $ nub $ outs) of
    1 -> return $ Right True
    _ -> return $ Right False
    -}


storeBDD :: BDD -> Node -> KS ()
storeBDD bdd n = do
  (g, m1, m2) <- get
  put (g, M.insert bdd n m1, M.insert n bdd m2)

deleteBDD :: Node -> KS ()
deleteBDD n = do
  (g, m1, m2) <- get
  put (g, m1, M.delete n m2)


kuelmannNode :: Node -> KS ()
kuelmannNode n1 =
  do (g, m1, m2) <- get
     mbdd <- calcBDDNode n1
     case mbdd of
       Nothing -> return () --tell $ "Could not create BDD for " ++ show n1
       Just bdd -> case M.lookup bdd m1 of
                     Nothing -> storeBDD bdd n1
                     Just n2 -> do
                       --lift $ tell $ [(g, "BDDs match: "++ show (n1, n2) ++ " -> "++ show bdd)]
                       nr <- mergeNodes n1 n2
                       storeBDD bdd nr

calcBDDNode' n = do
  mbdd <- calcBDDNode n
  case mbdd of
    Nothing -> return ()
    Just bdd -> storeBDD bdd n
  return mbdd

calcBDDNode :: Node -> KS (Maybe BDD)
calcBDDNode n = do
  g <- getG
  if indeg g n == 0
     then return $ Just $ case val g n of
                            Input _ -> initialBDD n
                            Wire _ -> error "should not be empty"
                            Output _ -> error "should not be empty"
                            ValZero -> bddZero
                            ValOne -> bddOne

     else do
       is <- mapM getBDDfromEdge (inn g n)
       return $ foldr bddAnd bddOne <$> sequence is


runKS :: [Node] -> G -> KS a -> (a, [String])
runKS is g m = (r, kuelLog ++ bddLog)
  --((a0, [String]), (BDDGraph.T, [(Node, Node)]))

 where
   (((r, kuelLog), bddLog), (bddGraphRes, eqs)) = runBDDState is $ flip evalStateT (g, M.empty, M.empty) (runWriterT m)

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97_2 :: Verilog -> Verilog -> (Either String Bool, [String])
equivKuelmann97_2 v1 v2 =
  runKS inputs g $ do mapM_ kuelmannNode todo
                      checkResult
  where g = makeGraphV [v1, v2]
        todo = mybfs g
        inputs = getInputs g
