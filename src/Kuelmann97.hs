{-# LANGUAGE FlexibleContexts #-}
module Kuelmann97 where

import Verilog
import Equivalence
import Graph
import GraphMonad
import BDDGraph
import BDDGraphMonad
import BDDGraphCommon

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

cashOut :: KS [(Node, Node)]
cashOut = do
  es <- equals <$> get
  modify $ \s -> s { equals = [] }
  return es

kuelmannNode :: Node -> KS ()
kuelmannNode n1 =
  do
    calcBDDNode n1
    o <- order <$> getG
    c <- getCount
    sz <- getSize
    --traceM (printf "Current Node: %5d -- %5d/%5d -- BDD Size: %5d" n1 c o sz)
    return ()

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
  where (t, g', _) = runKS g $ do mapM_ kuelmannNode (mybfs g)
                                  reduceAll

reduceG :: G -> G
reduceG g = g'
  where (_, g', _) = runKS g $ do mapM_ kuelmannNode (mybfs g)
                                  reduceAll

checkEquivRed :: G -> G -> G -> Bool
checkEquivRed g1 g2 gu =
  o1_s == o2_s && o1_s == ou_s
  where
    o1_s = length $ getOutputs g1
    o2_s = length $ getOutputs g2
    ou_s = length $ getOutputs gu

getPreds :: G -> Node -> (Node, [(Node, Bool)])
getPreds g y = (y, sort $ [(o, v) | (o, d, v) <- inn g y])

checkResult :: G -> Bool
checkResult g = r
  where ps = map (getPreds g) (getOutputs g)
        ps'  = sortBy (\a b -> snd a `compare` snd b) ps
        ps'' = groupBy (\a b -> snd a == snd b) ps'
        r = all (\x -> length x >= 2) ps''



