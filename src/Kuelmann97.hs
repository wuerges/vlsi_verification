{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Equivalence
import Graph
import BDD

import Control.Monad.State
import Control.Monad.Memo
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace
import qualified Data.Map as M
import qualified Data.IntMap as I

import qualified Data.Set as S

-- | Checks Equivalence of circuits based on Kuelmann97
{-
equivKuelmann97M :: Checker
equivKuelmann97M g1 g2 os1 os2 = --trace (showGraph g') $
                        checkExits g' os1 os2
  where g = g1 `union` g2
        todo = mybfs g
        g' = snd . snd $ evalState (runStateT (mapM_ kuelmannStep todo) (M.empty, g)) I.empty
-}
--type KS a = State (M.Map BDD Int, I.IntMap BDD, G) a

{-
getGraph :: KuelmannState G
getGraph = snd <$> get

putGraph :: RG -> KuelmannState ()
putGraph g = do (a, _, c) <- get
                put (a, g, c)
-}
{-
updateNode :: BDD -> Int -> KuelmannState ()
updateNode bdd i = do (m, g) <- get
                      put (M.insert bdd i m, g)

deleteNode :: BDD -> KuelmannState ()
deleteNode bdd = do (m, g) <- get
                    put (M.delete bdd m, g)
-}

-- | Merges 2 nodes in the graph. The first one is mantained, the second one is removed
-- | and all its sucessors are moved to the first one.
mergeNodes :: Node -> Node -> G -> G
mergeNodes n1 n2 g
  | gelem n1 g && gelem n2 g = (is1, n, v, os1 ++ os2) & g''
  | otherwise = g
  where
    (Just (is1, _, _, os1), g' )  = match n1 g
    (Just (is2, n, v, os2), g'')  = match n2 g'


type BDDState = State (M.Map Node BDD)
type KS = StateT (G, M.Map BDD Node) (MaybeT BDDState)

runBDD :: MaybeT BDDState a -> Maybe a
runBDD bs = evalState (runMaybeT bs) M.empty

runKS :: G -> KS a -> Maybe a
runKS g ks = fst <$> (runBDD $ runStateT ks (g, M.empty))

kuelmannNode :: Node -> KS ()
kuelmannNode n1 =
  do (g, m) <- get
     mbdd <- lift $ calcBDDNode g n1
     case M.lookup mbdd m of
       Nothing -> put (g, M.insert mbdd n1 m)
       Just n2 -> put (mergeNodes (max n1 n2) (min n1 n2) g, M.insert mbdd (max n1 n2) m)

getGraph :: KS G
getGraph = fst <$> get

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

equivKuelmann97_2 :: Checker
equivKuelmann97_2 g1 g2 os1 os2 =
  case g' of
    Nothing -> error "could not finish"
    Just x ->  sort (os2' \\ outputs x) == sort os2'
  where (g, os1', os2') = g1 `union` g2
        todo = mybfs g
        --Just (g', _, _) = foldM kuelmannNode (g, M.empty, M.empty) todo
        g' = runKS g $ do mapM_ kuelmannNode todo
                          getGraph

