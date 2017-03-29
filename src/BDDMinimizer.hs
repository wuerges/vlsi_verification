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

type MinT s = EquivT s D Node KS


runMinT :: MinT s a -> KS a
runMinT op = runEquivT (\n -> D n) minD op

leader :: Node -> MinT s D
leader n = classDesc n

minimize :: T -> (T, [(Node, Node)])
minimize = undefined

minimizeLayer :: [Node] -> MinT s ()
minimizeLayer ns = do
  mapM_ minimizeLayer1 ns

minimizeLayer1 :: Node -> MinT s ()
minimizeLayer1 n = do
  t <- lift getT
  let (z, o) = getSons t n
  when (gelem n t && outdeg t n > 0) $ do
    e <- equivalent z o
    when e $ E.equate n z


getEqSons :: Node -> MinT s (Node, (D, D))
getEqSons n = do
  (z, o) <- lift $ flip getSons n <$> getT
  zc <- leader z
  oc <- leader o
  return (n, (zc, oc))

minimizeLayer2 :: [Node] -> MinT s ()
minimizeLayer2 ns = do
  t <- lift getT
  let ns' = filter (flip gelem t) ns
  ns'' <- mapM getEqSons ns'
  let gs' = map (map fst) (sortAndGroupBy snd ns'')
  mapM_ equateAll gs'
