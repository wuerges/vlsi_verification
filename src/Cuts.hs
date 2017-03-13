module Cuts (retryEquivLimited_2) where

import Verilog
import Data.Graph.Inductive
import BDDGraph (getSize)
import Kuelmann97
import Graph
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Loops
import Data.List
import Debug.Trace



whileTodoM :: Monad m => (m Bool) -> (a -> m b) -> [a] -> m [b]
whileTodoM test action [] = return []
whileTodoM test action (t:ts) = do
  x <- test
  if x then do r <- action t
               rs <- whileTodoM test action ts
               return (r:rs)
       else return []

stopTest = do x <- liftX $ getSize
              return $ x < 3000

-- | Checks the equivalence o a small set of nodes.
-- | Marks the equivalent nodes to become inputs
equivLimited :: G -> [Node] -> (G, [Node], [String])
equivLimited g is = (g', eqs, log)
  where
    todo = mybfs g
    ns = nodes g
    ((eqs, g'), log) =
      runKS is ns g $ do m_eqs <- whileTodoM stopTest kuelmannNode todo
                         m_g <- getG
                         return (concat m_eqs, m_g)


--type RetryState = State ([Node], G)

rmdups = map head . group . sort

retryEquivLimited :: [Node] -> G -> Bool
retryEquivLimited is g  =
  trace ("Retrying: " ++ show (size g', size g)) $
    if (size g' >= size g) || checkResult g'
       then checkResult g'
       else retryEquivLimited (rmdups $ is ++ is') g'
 where (g', is', l) = equivLimited g is

retryEquivLimited_2 :: Verilog -> Verilog -> (Either String Bool, [String])
retryEquivLimited_2 v1 v2 = (Right r, [])
  where g = makeGraphV [v1, v2]
        r = retryEquivLimited (getInputs g) g

