module InputsMerger where

import BDDGraphCommon
import GraphMonad
import BDDGraphMonad
import BDDGraph
import Data.Graph.Inductive
import Graph
import Util
import Data.List
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

reduceWithInputs :: KS ()
reduceWithInputs = do
  g <- getG
  t <- getT
  let (_, g', t') = foldr reduceW1 (M.empty, g, t) (mybfs g)
  modifyG $ const g'
  modifyT $ const t'

  gs <- equivWithInputs <$> getG
  modifyG $ \g -> foldr mergeNodes' g gs
  modifyT $ \t -> foldr moveParents' t gs


equivWithInputs1 :: Node -> KS ()
equivWithInputs1 n = do
  ps <- sort . flip pre n <$> getG
  when (not $ null ps) $ do
    m <- inputMap <$> get
    case M.lookup ps m of
      Nothing -> modify $ \s -> s { inputMap = M.insert ps n m }
      Just x  -> do
        modify $ \s -> s { inputMap = M.insert ps (min n x) m }
        modifyG $ mergeNodes' (n, x)
        modifyT $ moveParents' (n, x)


equivWithInputs :: G -> [(Node, Node)]
equivWithInputs g = gs
  where
    n_is = map (\x -> (x, sort $ lpre g x)) $ nodes g
    n_is' = filter (\(a, b) -> not (null b)) $ n_is
    gs = concatMap (regroup . sort . map fst) $ sortAndGroupBy snd n_is'

reduceW1 :: Node ->
  (M.Map [Node] Node, G, T) ->
    (M.Map [Node] Node, G, T)
reduceW1 n (m, g, t)
  | null ps = (m, g, t)
  | otherwise      =
    case M.lookup ps m of
      Nothing -> ( M.insert ps n m, g, t)
      Just x  -> ( M.insert ps (min n x) m
                 , mergeNodes' (x, n) g
                 , moveParents' (x, n) t )
  where ps = pre g n

