{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Equivalence
import Graph
import BDDGraph (BDD, initialBDD, negateBDD)

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

type G2 = Gr (NT, Int, Maybe BDD) Bool

getBDD (_, _, bdd) = bdd

-- | Joins 2 graphs into one, merging the nodes with the same inputs.
union :: G -> G -> G2
union g1 g2 = g'
    where
      g1' = nmap (\nt -> (nt, 0, Nothing)) g1
      g2' = nmap (\nt -> (nt, 1, Nothing)) g2
      new_nodes = labNodes g1' ++ labNodes g2'
      new_edges = labEdges g1' ++ labEdges g2'
      g' = mkGraph new_nodes new_edges

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessors are moved to the first one.
-- | TODO Must check that always removes from same subgraph
mergeNodes :: Node -> Node -> G2 -> (Node, G2)
mergeNodes n1 n2 g = (n1, insEdges es' $ delEdges des g)
  where
    es = out g n2
    des = [(o, d) | (o, d, l) <- es]
    es' = [(n1, d, l) | (o, d, l) <- es]

mergeNodesM n1 n2 = do
  (g, m) <- get
  let (n, g') = mergeNodes n1 n2 g
  put (g', m)
  return n


--type KS a = WriterT String (State (G2, M.Map BDD Node)) a
type KS a = State (G2, M.Map BDD Node) a

checkResult :: KS (Either String Bool)
checkResult = do
  (g, m) <- get
  let outs = [or | (n, (nt, or, _)) <- labNodes g, outdeg g n == 0]
  case (length $ nub $ outs) of
    1 -> return $ Right True
    _ -> return $ Right False

storeBDD bdd n = do
  (g, m) <- get
  put (g, M.insert bdd n m)

kuelmannNode :: Node -> KS ()
kuelmannNode n1 =
  do (g, m) <- get
     case calcBDDNode g n1 of
       Nothing -> return () --tell $ "Could not create BDD for " ++ show n1
       Just bdd -> case M.lookup bdd m of
                     Nothing -> storeBDD bdd n1
                     Just n2 -> do nr <- mergeNodesM n1 n2
                                   storeBDD bdd nr

calcBDDNode :: G2 -> Node -> Maybe BDD
calcBDDNode g n = r
  where
    is = [(,) v <$> (getBDD . fromJust $ lab g o) | (o, d, v) <- inn g n]
    r = case sequence is of
          Just x -> Just $ mconcat $ map testNegate x
          Nothing -> Nothing

    testNegate (True, bdd) = bdd
    testNegate (False, bdd) = negateBDD bdd

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97_2 :: Checker
equivKuelmann97_2 v1 v2 = r
  where g = (makeGraphV v1) `union` (makeGraphV v2)
        todo = mybfs g
        r = flip evalState (g, M.empty) $ do
          mapM_ kuelmannNode todo
          checkResult
