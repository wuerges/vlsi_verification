{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Verilog
import Equivalence
import Graph
import BDDGraphMonad (BDDState, runBDDState
  , initialBDD_M, cashOut, bddAndMany
  , reduceAll, logBDD, getSize, negateBDDM )
import BDDGraph (bddZero, bddOne, BDD(B), BDDOrdering)

import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

import qualified Data.Set as S
import Text.Printf

type KS = WriterT [String] (StateT (G, M.Map BDD Node, M.Map Node BDD, Int) BDDState)

liftX :: BDDState a -> KS a
liftX = lift . lift

liftY :: BDDState a -> MaybeT KS a
liftY = lift . liftX

getNodeBddM :: KS (M.Map Node BDD)
getNodeBddM = do
  (_, _, m, _) <- get
  return m

getBddNodeM :: KS (M.Map BDD Node)
getBddNodeM = do
  (_, m, _, _) <- get
  return m

getCount :: KS Int
getCount = do
  (g, m1, m2, c) <- get
  put (g, m1, m2, c+1)
  return c

getBDD :: Node -> MaybeT KS BDD
getBDD n = do
  MaybeT $ M.lookup n <$> getNodeBddM


--getBDDfromEdge :: LEdge Bool -> MaybeT KS BDD
getBDDfromEdge :: LEdge Bool -> MaybeT KS BDD
getBDDfromEdge (o, _, v) =
  do bdd <- getBDD o
     if v then return bdd
          else liftY $ negateBDDM bdd

putG :: G -> KS ()
putG g = do
  (_, x, y, z) <- get
  put (g, x, y, z)

getG :: KS G
getG = do
  (g, _, _, _) <- get
  return g

-- | Merges 2 nodes in the graph.
-- | The left one is removed and the right one is mantained
-- | All the sucessors are moved to the node that remains.
-- | returns the remaining node
mergeNodes :: (Node, Node) -> KS Node
mergeNodes (n1, n2) = do
  g <- getG
  let [c1, c2] = sort [n1, n2]
      es = out g c2 -- getting the edges of n2
      des = [(o, d) | (o, d, l) <- es] -- preparing to remove the edges from n2
      es' = [(c1, d, l) | (o, d, l) <- es] --preparing to add the edges to n1
      g' = insEdges es' $ delEdges des g

  putG g'
  --liftX $ logBDD ("// before purge of " ++ show c2)
  purgeNode c2
  return n2
    {-
  g'' <- getG
  lift $ tell [("// before merge " ++ show (c1, c2) ++ "\n" ++ showGraph g ++ "\n")]
  lift $ tell [("// after merge " ++ show (c1, c2) ++ "\n" ++ showGraph g' ++ "\n")]
  lift $ tell [("// after purge " ++ show c2 ++ "\n" ++ showGraph g'' ++ "\n")]
  liftX $ logBDD ("after purge of " ++ show c2)
  -}


isWireOrInput n g = case l of
                      Wire _ -> True
                      Input _ -> True
                      _ -> False
  where Just l = lab g n

isWire n g = case l of
               Wire _ -> True
               _ -> False
  where Just l = lab g n

purgeNode' :: Node -> KS ()
purgeNode' n = do
  g <- getG
  putG (delNode n g)
  mapM_ purgeNode [o | (o, _, _) <- inn g n]

purgeNode :: Node -> KS ()
purgeNode n = do
  g <- getG
  when (gelem n g && isOutput g n) $ do
    --liftX $ bddPurge (B n)
    putG (delNode n g)
    mapM_ purgeNode [o | (o, _, _) <- inn g n]

getPreds :: G -> Node -> (Node, [(Node, Bool)])
getPreds g y = (y, sort $ [(o, v) | (o, d, v) <- inn g y])

checkResult :: G -> Bool
checkResult g = r
  where ps = map (getPreds g) (getOutputs g)
        ps'  = sortBy (\a b -> snd a `compare` snd b) ps
        ps'' = groupBy (\a b -> snd a == snd b) ps'
        r = all (\x -> length x >= 2) ps''

storeBDD :: BDD -> Node -> KS ()
storeBDD bdd n = do
  (g, m1, m2, c) <- get
  put (g, M.insert bdd n m1, M.insert n bdd m2, c)

deleteBDD :: Node -> KS ()
deleteBDD n = do
  (g, m1, m2, c) <- get
  put (g, m1, M.delete n m2, c)

kuelmannNode :: Node -> KS [Node]
kuelmannNode n1 =
  do
    g <- getG
    c <- getCount
    runMaybeT $ do bdd <- calcBDDNode n1
                   lift $ storeBDD bdd n1

    cash <- liftX $ reduceAll >> cashOut
    rem <- mapM mergeNodes cash
    -- TODO merge Nodes
    sz <- liftX getSize
    trace (printf "Current Node: %5d -- %5d/%5d -- BDD Size: %5d -- Cash Out %s" n1 c (size g) sz (show cash)) $ return rem
    return rem

calcBDDNode :: Node -> MaybeT KS BDD
calcBDDNode n = do
  g <- lift getG
  if indeg g n == 0
     then case n of
            0 -> return bddZero
            1 -> return bddOne
            _ -> liftY $ initialBDD_M n

     else do
       let inp_edges = (inn g n)
       is <- mapM getBDDfromEdge inp_edges
       liftY $ bddAndMany (Just n) is


--genOrdering :: G -> I.IntMap Node
genOrdering :: G -> BDDOrdering
genOrdering g = f
  where --is = S.fromList $ getInputs g
        --values = filter (\e -> S.member e is) (mybfs g)
        values = mybfs g
        m = I.fromList $ zip values [1..]
        f n1 n2 =
            let Just o1 = I.lookup n1 m
                Just o2 = I.lookup n2 m
             in o1 `compare` o2


runKS :: G -> KS a -> (a, [String])
runKS g m = (r, kuelLog ++ bddLog)

 where
   is = getInputs g
   ns = nodes g
   ord = genOrdering g
   (((r, kuelLog), bddLog), (bddGraphRes, eqs)) = runBDDState is ns ord $ flip evalStateT (g, M.empty, M.empty, 1) (runWriterT m)


equivKuelmann97_2 :: Verilog -> Verilog -> (Either String Bool, [String])
equivKuelmann97_2 v1 v2 = (Right (checkResult g'), log)
  where g = makeGraphV [v1, v2]
        todo = mybfs g
        (g', log) = runKS g $ do mapM_ kuelmannNode todo
                                 getG

equivG :: G -> (Bool, [String])
equivG g = (checkResult g', log)
  where (g', log) = runKS g $ do mapM_ kuelmannNode (mybfs g)
                                 getG


