module InputsMerger where

import BDDGraphCommon
import GraphMonad
import BDDGraphMonad
import BDDGraph
import Data.Graph.Inductive
import Graph
import Util
import Data.List

-- TODO this must be a fold
-- must not merge inputs



reduceWithInputs :: KS ()
reduceWithInputs = do
  gs <- equivWithInputs <$> getG
  modifyG $ \g -> foldr mergeNodes' g gs
  modifyT $ \t -> foldr moveParents' t gs


equivWithInputs :: G -> [(Node, Node)]
equivWithInputs g = gs
  where
    n_is = map (\x -> (x, sort $ lpre g x)) $ nodes g
    n_is' = filter (\(a, b) -> not (null b)) $ n_is
    gs = concatMap (regroup . sort . map fst) $ sortAndGroupBy snd n_is'
