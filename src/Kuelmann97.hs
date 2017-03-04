{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Equivalence
import Graph
import BDDGraph (BDD, initialBDD, negateBDD)

import Control.Monad.State
import Control.Monad.Memo
import Control.Monad.Trans.Class

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

import qualified Data.Set as S

type G2 = Gr (NT, Int, Maybe BDD) Bool
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
mergeNodes :: Node -> Node -> G2 -> G2
mergeNodes n1 n2 g = insEdges es' $ delEdges des g
  where
    es = out g n2
    des = [(o, d) | (o, d, l) <- es]
    es' = [(n1, d, l) | (o, d, l) <- es]


--type BDDState = State (M.Map Node BDD)
type KS a = State (G2, M.Map BDD Node) a

--runBDD :: MaybeT BDDState a -> Maybe a
--runBDD bs = evalState (runMaybeT bs) M.empty

checkResult :: KS (Either String Bool)
checkResult = return $ Left "Dummy check"

kuelmannNode :: Node -> KS ()
kuelmannNode n1 =
  do (g, m) <- get
     let Just (nt, ord, bdd) = lab g n1
     return ()
       {-

     mbdd <- lift $ calcBDDNode g n1
     case M.lookup mbdd m of
       Nothing -> put (g, M.insert mbdd n1 m)
       Just n2 -> put (mergeNodes (max n1 n2) (min n1 n2) g, M.insert mbdd (max n1 n2) m)
-}

getGraph :: KS G2
getGraph = fst <$> get

{-
bddLookup :: Node -> BDDState (Maybe BDD)
bddLookup n = M.lookup n <$> get

bddPut :: Node -> BDD -> BDDState BDD
bddPut n bdd = do modify (M.insert n bdd)
                  return bdd

calcBDDNode :: G -> Node -> MaybeT BDDState BDD
calcBDDNode g n =
  do mbdd <- lift $ bddLookup n
     case mbdd of
       Just bdd -> return bdd
       Nothing ->
         do (is, n, nv, os) <- MaybeT . return $ fst $ match n g
            calcBDDEdges g n is

calcBDDEdges :: G -> Node -> [(Bool, Node)] -> MaybeT BDDState BDD
calcBDDEdges _ n [] = lift $ bddPut n (initialBDD n)
calcBDDEdges g n is = do bdds <- mapM (calcBDDEdge g) is
                         lift $ bddPut n (mconcat bdds)

calcBDDEdge :: G -> (Bool, Node) -> MaybeT BDDState BDD
calcBDDEdge g (v, n)
  | v         =             calcBDDNode g n
  | otherwise = negateBDD <$> calcBDDNode g n
-}
-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97_2 :: Checker
equivKuelmann97_2 v1 v2 = r
  where g = (makeGraphV v1) `union` (makeGraphV v2)
        todo = mybfs g
        r = flip evalState (g, M.empty) $ do
          mapM_ kuelmannNode todo
          checkResult


