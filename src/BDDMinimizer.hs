 {-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module BDDMinimizer where

import Control.Monad.State.Strict

import qualified UnionFind as U
import qualified Data.IntMap as IM
import Data.Graph.Inductive
import Graph
import BDDGraphCommon
import BDDGraph
import BDDGraphMonad


type Pt = U.Point Node

data MS = MS { supply :: U.PointSupply Int
             , ptMap  :: IM.IntMap Pt }

type MinT = StateT MS KS

fresh :: Node -> MinT Pt
fresh n = do
  s <- get
  case IM.lookup n (ptMap s) of
    Just x -> return x
    Nothing -> let (ps', p') = U.fresh (supply s) n
                in do put $ MS ps' (IM.insert n p' (ptMap s))
                      return p'

leader :: Node -> MinT Pt
leader n = do
  f <- fresh n
  s <- get
  return $ U.repr (supply s) f

equivalent :: Node -> Node -> MinT Bool
equivalent n1 n2 = do
  p1 <- fresh n1
  p2 <- fresh n2
  s <- get
  return $ U.equivalent (supply s) p1 p2

union :: (Node, Node) -> MinT ()
union (n1, n2) = do
  p1 <- fresh n1
  p2 <- fresh n2
  s <- get
  let ps' = U.union (supply s) p1 p2
  put $ s { supply = ps' }

minimize :: T -> (T, [(Node, Node)])
minimize = undefined

minimizeLayer :: [Node] -> MinT ()
minimizeLayer ns = do
  mapM_ minimizeLayer1 ns

minimizeLayer1 :: Node -> MinT ()
minimizeLayer1 n = do
  t <- lift getT
  let (z, o) = getSons t n
  when (gelem n t && outdeg t n > 0) $ do
    e <- equivalent z o
    when e $ union (n, z)


getEqSons :: Node -> MinT (Node, (Pt, Pt))
getEqSons n = do
  (z, o) <- lift $ flip getSons n <$> getT
  zc <- leader z
  oc <- leader o
  return (n, (zc, oc))

minimizeLayer2 :: [Node] -> MinT ()
minimizeLayer2 ns = do
  t <- lift getT
  let ns' = filter (flip gelem t) ns
  ns'' <- mapM getEqSons ns'
  let gs' = concatMap regroup $ map (map fst) (sortAndGroupBy snd ns'')
  mapM_ union gs'
