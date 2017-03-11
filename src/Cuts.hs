module Cuts where

import Data.Graph.Inductive
import Kuelmann97
import Graph
import Control.Monad.State
import Control.Monad.Writer

-- | Checks the equivalence o a small set of nodes.
-- | Marks the equivalent nodes to become inputs
equivLimited :: G -> [Node] -> (G, [Node], [String])
equivLimited g is = (g', eqs, log)
  where
    todo = take 1000 $ mybfs g
    ns = nodes g
    ((eqs, g'), log) =
      runKS is ns g $ do m_eqs <- mapM kuelmannNode todo
                         m_g <- getG
                         return (concat m_eqs, m_g)


