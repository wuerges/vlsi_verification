 {-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module BDDMinimizer where

import Control.Monad
import Control.Monad.Trans
import Data.Equivalence.Monad as E
import Data.Graph.Inductive
import Graph
import BDDGraphCommon
import BDDGraph
import Util
--import BDDGraphMonad


newtype D = D Int deriving (Eq, Ord)

minD :: D -> D -> D
minD (D a) (D b) = D $ min a b

type Min s = EquivM s D Node

leader :: Node -> Min s D
leader n = classDesc n


isLeader :: Node -> Min s Bool
isLeader n = do
  D x <- leader n
  return $ x == n

checkNode (a, _) = isLeader a
checkEdge (a, _, _) = isLeader a

remapEdge (a, b, l) = do
  D a' <- leader a
  D b' <- leader b
  return (a', b', l)


minimizeT :: T -> Min s T
minimizeT t =
  let ns = labNodes t
      es = labEdges t
   in do ns' <- filterM checkNode ns
         es' <- filterM checkEdge es
         es'' <- mapM remapEdge es'
         return $ mkGraph ns' (rmdups es'')

getEquiv n = do
  D x <- leader n
  return (x, n)

runMinimize :: T -> (T, [(Node, Node)])
runMinimize t = runEquivM D minD (minimize t)

minimize :: T -> Min s (T, [(Node, Node)])
minimize t =
  let ls = layers t
   in do mapM_ (minimizeLayer1 t >> minimizeLayer2 t) ls
         t' <- minimizeT t
         ns' <- filterM (\n -> isLeader n >>= return . not) (nodes t)
         --ns' <- filterM (\n -> do { x <- isLeader n; return $ not x}) (nodes t)
         eqs <- mapM getEquiv ns'
         return (t', rmdups eqs)


minimizeLayer1 :: T -> [Node] -> Min s ()
minimizeLayer1 t ns = do
  mapM_ (minimizeLayer1n t) ns

minimizeLayer1n :: T -> Node -> Min s ()
minimizeLayer1n t n = do
  let (z, o) = getSons t n
  when (gelem n t && outdeg t n > 0) $ do
    e <- equivalent z o
    when e $ E.equate n z


getEqSons :: T -> Node -> Min s (Node, (D, D))
getEqSons t n =
  let (z, o) = getSons t n
   in do zc <- leader z
         oc <- leader o
         return (n, (zc, oc))

minimizeLayer2 :: T -> [Node] -> Min s ()
minimizeLayer2 t ns =
  let ns' = filter (flip gelem t) ns
   in do ns'' <- mapM (getEqSons t) ns'
         let gs' = map (map fst) (sortAndGroupBy snd ns'')
         mapM_ equateAll gs'


