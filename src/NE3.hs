module NE3 where

import Graph
import BDD


type NE3State a = State Int a

-- | Removes a Node from the graph, adding its BDD to the Monad
-- | and returning the new graph and the BDD
representation :: Int -> G -> BDDState (G, BDD)
representation = undefined

-- | BDD e' uma ideia ruim
-- | Talvez calcular o numero de resultados Trues seja melhor
