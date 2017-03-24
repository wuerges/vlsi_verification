module BDDGraphCommon where

import Data.Graph.Inductive
import Control.Monad.State.Strict

data KS_D = S { bdd :: T
              , ordering :: BDDOrdering
              , count :: Int
              , graph :: G
              , equals :: [(Node, Node)]
              , cuts :: [Node] }

type KS = State KS_D

newtype BDD = B Int
  deriving (Eq, Ord, Show)

data V =  V { input :: Node
            , repr :: Bool }
  deriving Show

type G = Gr String Bool
type CtxG = Context String Bool
type CtxT = Context V Bool

type T = Gr V Bool
type BDDOrdering = (Node -> Node -> Ordering)


getG :: KS G
getG = graph <$> get

