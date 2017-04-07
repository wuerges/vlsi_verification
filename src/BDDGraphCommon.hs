module BDDGraphCommon where

import Data.Graph.Inductive
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Set(Set)
import qualified Data.Set as S
import Control.Monad.State.Strict

data KS_D = S { bdd :: T
              , ordering :: BDDOrdering
              , count :: Int
              , graph :: G
              , inputMap :: Map [Node] Node
              , visited :: Set Node
              , equals :: [(Node, Node)]
              , cuts :: [Node] }


startingKS :: KS_D
startingKS =
  S empty undefined 0 empty M.empty S.empty [] []


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

