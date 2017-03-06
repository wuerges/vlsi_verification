--module BDDGraph (BDD, initialBDD, negateBDD ) where
module BDDGraph where

import Graph
import Debug.Trace
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS (reachable)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.List (intersperse, sortOn, groupBy)
import qualified Data.Set as S

newtype BDD = B Int
  deriving Show

data V =  V { input :: Node
            , repr :: Nod }
  deriving (Eq, Ord, Show)

type T = Gr V Bool
type Ctx = Context V Bool

type BDDStateT a = State T a

startingG = mkGraph [(0,V (-1)), (1, V (-1))] [] :: T

runBDDStateT is op = flip runStateT startingG op

initialBDD :: Node -> BDDStateT BDD
initialBDD n = do
  g <- get
  unless (gelem n g) $ do
    modify $ insNode (n, V n)
    modify $ insEdges [(n, 0, False), (n, 1, True)]
  return $ B n

-- | Exported function
bddOne = B 1
-- | Exported function
bddZero = B 0

bval :: Node -> BDDStateT V
bval o = do
  Just v <- flip lab o <$> get
  return v

dupNode :: V -> BDDStateT Node
dupNode v = do
  [n] <- newNodes 1 <$> get
  modify $ insNode (n, v)
  return n

getSons :: Node -> BDDStateT (Node, Node)
getSons n = do
  es <- flip out n <$> get
  case es of
    [(_, l, False), (_, r, True)] -> return (l, r)
    [(_, r, False), (_, l, False)] -> return (l, r)

getL :: Node ->  BDDStateT Node
getL n = fst <$> getSons n
getR :: Node ->  BDDStateT Node
getR n = snd <$> getSons n

newParent :: V -> (Node, Node) -> BDDStateT BDD
newParent n (l, r) = do
  n' <- dupNode n
  modify $ insEdges [(n', l, False), (n', r, True)]
  return $ B n'

-- | Exported function
negateBDD :: BDD -> BDDStateT BDD
negateBDD (B 0) = return $ B 1
negateBDD (B 1) = return $ B 0
negateBDD (B n) = do
  v <- bval n
  (l, r) <- getSons n
  B l' <- negateBDD $ B l
  B r' <- negateBDD $ B r
  newParent v (l', r')

bddAnd :: BDD -> BDD -> BDDStateT BDD
bddAnd (B 0) _ = return $ B 0
bddAnd _ (B 0) = return $ B 0
bddAnd (B 1) b = return b
bddAnd b (B 1) = return b

bddAnd (B n1) (B n2) = do
  v_n1 <- bval n1
  v_n2 <- bval n2
  (z1, o1) <- getSons n1
  (z2, o2) <- getSons n2

  case v_n1 `compare` v_n2 of
    GT -> do
      B z <- bddAnd (B z1) (B n2)
      B o <- bddAnd (B o1) (B n2)
      newParent v_n1 (z, o)
    LT -> do
      B z <- bddAnd (B z2) (B n1)
      B o <- bddAnd (B o2) (B n1)
      newParent v_n2 (z, o)
    EQ -> do
      B z <- bddAnd (B z1) (B z2)
      B o <- bddAnd (B o1) (B o2)
      newParent v_n1 (z, o)

setSons n z o =
  modify $ insEdges [(n, z, False), (n, o, True)]

reduce1 :: BDD -> BDDStateT ()
reduce1 (B 0) = return ()
reduce1 (B 1) = return ()
reduce1 (B n) = do
  (z, o) <- getSons n
  when (z == o) $ do
    modify $ delEdges [(n,z), (n,o)]
    (z', o') <- getSons z
    setSons n z' o'

reduce2 :: BDD -> BDD -> BDDStateT ()
reduce2 (B 0) _ =  return ()
reduce2 _ (B 0) = return ()
reduce2 (B 1) _ = return ()
reduce2 _ (B 1) = return ()

reduce2 (B n1) (B n2) = do
  (z1, o1) <- getSons n1
  (z2, o2) <- getSons n2
  when (z1 == z2 && o1 == o2) $ do
    moveParents n1 n2

moveParents :: Node -> Node -> BDDStateT ()
moveParents n1 n2 = do
  ps_n1 <- flip inn n1 <$> get
  modify $ insEdges [(o, n2, v) | (o,_,v) <- ps_n1]
