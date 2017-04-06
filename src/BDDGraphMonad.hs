module BDDGraphMonad where

import Graph
import GraphMonad
import BDDGraph
import BDDGraphCommon
import BDDMinimizer

import Data.Graph.Inductive
import Control.Monad.State.Strict
--import Debug.Trace
import qualified Data.IntMap as I
import Util

data Op = Op Node Node

mkOp :: Node -> Node -> Op
mkOp a b = Op (min a b) (max a b)

getRepr :: Node -> KS Bool
getRepr n = do
  t <- getT
  return $ maybe False repr (lab t n)

equate :: Node -> Node -> KS ()
equate n1 n2 = --trace ("Equating " ++ show (n1, n2)) $
  do
  --r1 <- getRepr n1
  --r2 <- getRepr n2
    --when (r1 && r2) $ do
      modify $ \s -> s { equals = (n1, n2):(equals s) }
      mergeNodes n1 n2

genOrdering :: G -> BDDOrdering
genOrdering g = f
  where values = mybfs g
        m = I.fromList $ zip values [(1::Int)..]
        f n1 n2 =
            let Just o1 = I.lookup n1 m
                Just o2 = I.lookup n2 m
             in --traceShow ("order", n1, n2) $
               o1 `compare` o2

calcBDDNode :: Node -> KS ()
calcBDDNode n = do
  g <- getG
  _ <- if indeg g n == 0
         then case n of
                0 -> return $ B 0
                1 -> return $ B 1
                _ -> initialBDD_M n

         else do is <- mapM getBDDfromEdge (inn g n)
                 bddAndMany (Just n) is
  return ()

getCount :: KS Int
getCount = do
  modify $ \s -> s { count = count s + 1 }
  count <$> get

getBDD :: Node -> BDD
getBDD n = B n

getBDDfromEdge :: LEdge Bool -> KS BDD
getBDDfromEdge (o, _, v) =
  if v then return $ B o
       else negateBDDM $ B o

withBDD :: T -> (KS a) -> T
withBDD t op =
  evalState (op >> getT) $ S t compare 0 (empty :: G) [] []

runKS :: G -> (KS a) -> (T, G, a)
runKS g op = r'
  where
    order_ = genOrdering g
    initialT = reserveNodes (nodes g) startingT
    r' = flip evalState (S initialT order_ 0 g [] []) $ do
      r <- op
      t <- getT
      g_ <- getG
      return (t, g_, r)

getT :: KS T
getT =  bdd <$> get

modifyT :: (T -> T) -> KS ()
modifyT f = do
  s <- get
  put $ s { bdd = f (bdd s) }

bddAndMany :: Maybe Node -> [BDD] -> KS BDD
bddAndMany _ [] = error $ "cannot conjoin nothing"
bddAndMany n [B b] = bddAnd n (B b) (B 1)
bddAndMany n [a, b] = bddAnd n a b
bddAndMany n (a:os) = do
  r <- bddAndMany Nothing os
  bddAnd n a r

bddAndRepr :: Node -> BDD -> BDD ->  KS BDD
bddAndRepr n b1 b2 = do
  bddAnd (Just n) b1 b2

getOrdering :: Node -> Node -> KS Ordering
getOrdering n1 n2 = do
  m <- ordering <$> get
  return $ n1 `m` n2

newParentM :: Maybe Node -> Node -> (Node, Node) -> KS BDD
newParentM repr_ orig (l, r) = do
  g <- getT
  let (bdd', g') = newParent repr_ orig (l,r) g
  modifyT $ const g'
  return bdd'

bddAnd :: Maybe Node -> BDD -> BDD -> KS BDD

-- Trivial cases when there is no representative
bddAnd Nothing (B 0) _     = return $ B 0
bddAnd Nothing (B 1) (B b) = return $ B b

-- When there is a representative it must be
-- added to the BDD no matter the cost.
bddAnd (Just x) (B 0) _ = do
  --equate 0 x
  --return $ B 0
  newParentM (Just x) 0 (0, 0)

bddAnd (Just x) (B 1) (B b) = do
  --equate x b
  --return $ B (min x b)
  newParentM (Just x) b (b, b)

bddAnd repr_ _ (B 0) = bddAnd repr_ (B 0) undefined
bddAnd repr_ b (B 1) = bddAnd repr_ (B 1) b

bddAnd repr_ (B n1) (B n2) = do
  v_n1 <- inputNode n1 <$> getT
  v_n2 <- inputNode n2 <$> getT
  (z1, o1) <- flip getSons n1 <$> getT
  (z2, o2) <- flip getSons n2 <$> getT

  ord <- getOrdering v_n1 v_n2
  case ord of
  --case v_n1 `compare` v_n2 of
    GT -> do
      B z <- bddAnd Nothing (B z1) (B n2)
      B o <- bddAnd Nothing (B o1) (B n2)
      newParentM repr_ n1 (z, o)
    LT -> do
      B z <- bddAnd Nothing (B z2) (B n1)
      B o <- bddAnd Nothing (B o2) (B n1)
      newParentM repr_ n2 (z, o)
    EQ -> do
      B z <- bddAnd Nothing (B z1) (B z2)
      B o <- bddAnd Nothing (B o1) (B o2)
      newParentM repr_ n1 (z, o)

setSons n z o =
  modifyT $ insEdges [(n, z, False), (n, o, True)]

reduce1' :: Node -> (T, [(Node,Node)]) -> (T, [(Node,Node)])
reduce1' n (t, l) =
  if gelem n t && outdeg t n > 0 && z == o
     then t' `seq` (t', (n,z):l)
     else (t, l)
  where
    t' = moveParents' (n,z) t
    (z, o) = getSons t n

reduce1 :: BDD -> KS ()
reduce1 (B n) = do
  t <- getT
  let (t', es) = reduce1' n (t, [])
      equate' (a, b) = equate a b
  modifyT $ const t'
  mapM_ equate' es

reduceLayer :: [Node] -> KS [(Node, Node)]
reduceLayer ls = do
  t0 <- getT
  --let (te, eqs1) = foldl' (flip reduce1') (t0, []) ls
  let (te, eqs1) = foldr reduce1' (t0, []) ls
  modifyT $ const te

  eqs2 <- reduce2Layer ls
  --modifyT $ \t' -> foldl' (flip moveParents') t' eqs2
  modifyT $ \t' -> foldr moveParents' t' eqs2
  return $ eqs1 ++ eqs2


getSize :: KS Int
getSize = order <$> getT


reduce2Layer :: [Node] -> KS [(Node,Node)]
reduce2Layer ls = do
  t <- getT
  return $ concatMap regroup (groupWithSons t ls)

reduceAll :: KS ()
reduceAll = do
  t <- getT
  eqs <- mapM reduceLayer $! (reverse $ layers t)
  let eqs' = concat eqs
  --let (t', eqs') = runMinimize t
  --modifyT $ const t'
  modifyG $ \g' -> foldr mergeNodes' g' eqs'


-- Monadic functions show be bellow here
initialBDD_M :: Node -> KS BDD
initialBDD_M n =
  do modifyT $ initialBDD n
     return $ B n

negateBDDM b = do
  (bdd', g') <- negateBDD b <$> getT
  modify $ \s -> s { bdd = g' }
  return bdd'


