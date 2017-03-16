module Cuts where

import Verilog
import Data.Graph.Inductive
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Query.DFS
import BDDGraphMonad (getSize)
import Kuelmann97
import Graph
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Loops
import Data.List
import Debug.Trace
import qualified Data.IntMap as I
import qualified Data.Set as S
import Data.Maybe


whileTodoM :: Monad m => (m Bool) -> (a -> m b) -> [a] -> m [b]
whileTodoM test action [] = return []
whileTodoM test action (t:ts) = do
  x <- test
  if x then do r <- action t
               rs <- whileTodoM test action ts
               return (r:rs)
       else return []

stopTest = do x <- liftX $ getSize
              return $ x < 10000

-- | Checks the equivalence o a small set of nodes.
-- | Marks the equivalent nodes to become inputs
equivLimited :: G -> (G, [Node], [String])
equivLimited g = (g', eqs, log)
  where
    todo = mybfs g
    ((eqs, g'), log) =
      runKS g $ do
        m_eqs <- whileTodoM stopTest kuelmannNode todo
        m_g <- getG
        return (concat m_eqs, m_g)

cutLevelsGraph :: G -> [Node] -> I.IntMap Node
cutLevelsGraph g cuts = foldr doNode I.empty (mybfs g)
  where
    s = S.fromList cuts
    level m n = maybe 0 id (I.lookup n m)
    cutLevel n m = foldr max 0 (map (level m) (pre g n))
    cutLevel' n m
      | S.member n s = cutLevel n m + 1
      | otherwise = cutLevel n m
    doNode n m = I.insert n (cutLevel n m + (if S.member n s then 1 else 0)) m



cutGraph :: [Node] -> G -> G
cutGraph is g = sg --gmap changeInput sg
  where
    os = getOutputs g
    rem = dfs os (grev g)
    sg = subgraph rem g

--rmdups = map head . group . sort

retryEquivLimited :: G -> Bool
retryEquivLimited g  =
  trace ("Retrying: " ++ show (order g', order g)) $
    if (order g' >= order g) || checkResult g'
       then checkResult g'
       else retryEquivLimited cg
 where (g', is', l) = equivLimited g
       cg = cutGraph is' g'

retryEquivLimited_2 :: Verilog -> Verilog -> (Either String Bool, [String])
retryEquivLimited_2 v1 v2 = (Right r, [])
  where g = makeGraphV [v1, v2]
        r = retryEquivLimited  g

