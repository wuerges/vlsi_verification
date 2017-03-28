module Cuts where

import Verilog
import Data.Graph.Inductive
import BDDGraphMonad
import BDDGraphCommon
import Kuelmann97
import Graph
import Debug.Trace
import qualified Data.IntMap as I
import qualified Data.Set as S


whileTodoM :: Monad m => (m Bool) -> (a -> m b) -> [a] -> m [b]
whileTodoM _ _ [] = return []
whileTodoM test action (t:ts) = do
  x <- test
  if x then do r <- action t
               rs <- whileTodoM test action ts
               return (r:rs)
       else return []

stopTest :: KS Bool
stopTest = do x <- getSize
              return $ x < 10000

-- | Checks the equivalence o a small set of nodes.
-- | Marks the equivalent nodes to become inputs
equivLimited :: G -> (G, [Node])
equivLimited g = (g', [])
  where
    todo = mybfs g
    (_, g', _) =
      runKS g $ do
        whileTodoM stopTest kuelmannNode todo

cutLevelsGraph :: G -> [Node] -> I.IntMap Node
cutLevelsGraph g cuts_ = foldr doNode I.empty (mybfs g)
  where
    s = S.fromList cuts_
    level_ m n = maybe 0 id (I.lookup n m)
    cutLevel n m = foldr max 0 (map (level_ m) (pre g n))
    --cutLevel' n m
    --  | S.member n s = cutLevel n m + 1
    --  | otherwise = cutLevel n m
    doNode n m = I.insert n (cutLevel n m + (if S.member n s then 1 else 0)) m



cutGraph :: [Node] -> G -> G
cutGraph _ g = sg --gmap changeInput sg
  where
    os = getOutputs g
    rem_ = dfs os (grev g)
    sg = subgraph rem_ g

retryEquivLimited :: G -> Bool
retryEquivLimited g  =
  trace ("Retrying: " ++ show (order g', order g)) $
    if (order g' >= order g) || checkResult g'
       then checkResult g'
       else retryEquivLimited cg
 where (g', is') = equivLimited g
       cg = cutGraph is' g'

retryEquivLimited_2 :: Verilog -> Verilog -> (Either String Bool, [String])
retryEquivLimited_2 v1 v2 = (Right r, [])
  where g = makeGraphV [v1, v2]
        r = retryEquivLimited  g

