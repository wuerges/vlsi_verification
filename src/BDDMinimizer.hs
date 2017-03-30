 {-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module BDDMinimizer where

import Control.Monad
import Control.Monad.Trans
import Data.Equivalence.Monad as E
import Data.Graph.Inductive
import Graph
import BDDGraphCommon
import BDDGraph
import BDDGraphMonad


newtype D = D Int deriving (Eq, Ord)

minD :: D -> D -> D
minD (D a) (D b) = D $ min a b

type Min s = EquivM s D Node

leader :: Node -> Min s D
leader n = classDesc n

minimize :: T -> Min s (T, [(Node, Node)])
minimize t =
  let ls = layers t
   in do mapM_ (minimizeLayer1 t >> minimizeLayer2 t) ls
         return (t, [])


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

