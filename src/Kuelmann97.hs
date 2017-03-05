{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Verilog
import Equivalence
import Graph
import BDD --Graph (BDD, initialBDD, negateBDD, bddOne, bddAnd)

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
type KS a = State (G, M.Map BDD Node, M.Map Node BDD) a

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

  put (insEdges es' $ delEdges des g, m1, m2)
  return $ trace ("Merged " ++ show (n1, n2)) $ n1

checkResult :: KS (Either String Bool)
checkResult =  do
  (g, m1, m2) <- get
  return $
    error $
      "uninplemented" ++ show [(n, l) | (n, l) <- labNodes g, outdeg g n == 0] ++
        showGraph g
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
                     Just n2 -> do nr <- mergeNodes n1 n2
                                   storeBDD bdd nr

calcBDDNode :: Node -> KS (Maybe BDD)
calcBDDNode n = do
  g <- getG
  if indeg g n == 0
     then return $ Just $ case val g n of
                            Wire _ -> initialBDD n
                            ValZero -> bddZero
                            ValOne -> bddOne

     else do
       is <- mapM getBDDfromEdge (inn g n)
       return $ foldr bddAnd bddOne <$> sequence is

-- | Checks Equivalence of circuits based on Kuelmann97
equivKuelmann97_2 :: Checker
equivKuelmann97_2 v1 v2 = r
  where g = makeGraphV [v1, v2]
        todo = mybfs g
        r = flip evalState (g, M.empty, M.empty) $ do
          mapM_ kuelmannNode todo
          checkResult

