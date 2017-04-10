{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Verilog
--import Equivalence
import Graph
import GraphMonad
import BDDGraphMonad
import BDDGraphCommon
import InputsMerger

import Control.Monad.State
import Data.List hiding (union)
import Data.Graph.Inductive
import Debug.Trace

import           Data.Set(Set)
import qualified Data.Set as S
import Text.Printf

cashOut :: KS [(Node, Node)]
cashOut = do
  es <- equals <$> get
  modify $ \s -> s { equals = [] }
  return es

controlVisit :: Node -> KS ()
controlVisit n = do
  vs <- visited <$> get
  unless (S.member n vs) $ do
    modify $ \s -> s { visited = S.insert n (visited s) }
    ps <- flip pre n <$> getG
    mapM_ controlVisit ps
    kuelmannNode n

kuelmannNode :: Node -> KS ()
kuelmannNode n1 = do
  --g00 <- getG
  --when (gelem n1 g00) $ equivWithInputs1 n1
  --modifyG cleanDupEdges
  g0 <- getG
  when (gelem n1 g0) $ do
    calcBDDNode n1
    o <- order <$> getG
    c <- getCount
    sz <- getSize
    traceM (printf "Current Node: %5d -- %5d/%5d -- BDD order: %5d -- Graph order: %5d " n1 c o sz o  ++ show (sz > 20000))
    reduce1 (B n1)
    reduceAll
    --reduceWithInputs
    --when (sz > 20001) $ do
    --  reduceAll

equivVerilog :: Verilog -> Verilog -> Either String Bool
equivVerilog v1 v2 = Right $ checkEquivRed g1 g2 red
  where g1 =  makeGraphV [v1]
        g2 =  makeGraphV [v2]
        gu = joinGraphs g1 g2
        red = reduceG gu


equivG :: G -> Either String Bool
equivG g = Right $ checkResult (reduceG g)

reduceGT :: G -> (G, T)
reduceGT g = (g', t)
  where (t, g', _) = runKS g $ do
                       --reduceWithInputs
                       --g0 <- getG
                       mapM_ kuelmannNode (mybfs g)
                       reduceAll

reduceG :: G -> G
reduceG = fst . reduceGT

checkEquivRed :: G -> G -> G -> Bool
checkEquivRed g1 g2 gu =
  --o1_s == o2_s && o1_s == ou_s
  --traceShow (is, o1_s, o2_s) $
  (S.size is <= S.size o1_s) &&
  (S.size is <= S.size o2_s)
  where
    g2' = renumberGraph' g1 g2
    o1_s = S.fromList (getOutputs g1) `S.difference` zo
    o2_s = S.fromList (getOutputs g2') `S.difference` zo
    ou_s = S.fromList (getOutputs gu) `S.difference` zo
    is = (o1_s `S.union` o2_s) `S.intersection` ou_s
    zo = S.fromList [0,1]



getPreds :: G -> Node -> (Node, [(Node, Bool)])
getPreds g y = (y, sort $ [(o, v) | (o, _, v) <- inn g y])

checkResult :: G -> Bool
checkResult g = r
  where ps = map (getPreds g) (getOutputs g)
        ps'  = sortBy (\a b -> snd a `compare` snd b) ps
        ps'' = groupBy (\a b -> snd a == snd b) ps'
        r = all (\x -> length x >= 2) ps''



