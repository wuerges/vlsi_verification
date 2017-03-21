module BDDGraphMonad where

import Graph
import GraphMonad
import BDDGraph
import BDDGraphCommon

import Control.Monad.Writer
import Data.Graph.Inductive
import Control.Monad.State
import Data.Ord
import Data.List
import Debug.Trace
import Data.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.IntMap as I

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
        m = I.fromList $ zip values [1..]
        f n1 n2 =
            let Just o1 = I.lookup n1 m
                Just o2 = I.lookup n2 m
             in o1 `compare` o2

calcBDDNode :: Node -> KS ()
calcBDDNode n = do
  g <- getG
  if indeg g n == 0
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
    order = genOrdering g
    initialT = reserveNodes (nodes g) startingT
    r' = flip evalState (S initialT order 0 g [] []) $ do
      r <- op
      t <- getT
      g <- getG
      return (t, g, r)

getT :: KS T
getT =  bdd <$> get

modifyT :: (T -> T) -> KS ()
modifyT f = do
  s <- get
  put $ s { bdd = f (bdd s) }

bddAndMany :: Maybe Node -> [BDD] -> KS BDD
bddAndMany n [] = error $ "cannot conjoin nothing"
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
newParentM repr orig (l, r) = do
  g <- getT
  let (bdd', g') = newParent repr orig (l,r) g
  modifyT $ const g'
  return bdd'

bddAnd :: Maybe Node -> BDD -> BDD -> KS BDD

-- Trivial cases when there is no representative
bddAnd Nothing (B 0) _     = return $ B 0
bddAnd Nothing (B 1) (B b) = return $ B b

-- When there is a representative it must be
-- added to the BDD no matter the cost.
bddAnd (Just x) (B 0) _ =
  newParentM (Just x) 0 (0, 0)

bddAnd (Just x) (B 1) (B b) =
  newParentM (Just x) b (b, b)

bddAnd repr _ (B 0) = bddAnd repr (B 0) undefined
bddAnd repr b (B 1) = bddAnd repr (B 1) b

bddAnd repr (B n1) (B n2) = do
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
      newParentM repr n1 (z, o)
    LT -> do
      B z <- bddAnd Nothing (B z2) (B n1)
      B o <- bddAnd Nothing (B o2) (B n1)
      newParentM repr n2 (z, o)
    EQ -> do
      B z <- bddAnd Nothing (B z1) (B z2)
      B o <- bddAnd Nothing (B o1) (B o2)
      newParentM repr n1 (z, o)

setSons n z o =
  modifyT $ insEdges [(n, z, False), (n, o, True)]

reduce1 :: BDD -> KS ()
reduce1 (B 0) = return ()
reduce1 (B 1) = return ()
reduce1 (B n) = do
  t <- getT
  when (gelem n t) $ do
    let (z, o) = getSons t n
    when (z == o) $ do
      --modifyT $ delEdges [(n,z), (n,o)]
      mergeNodes1 n z

mergeNodes1 :: Node -> Node -> KS ()
mergeNodes1 top bot = do
  g <- getT
  let (Just (is_top, _, V _   r_top, _), g') = match top g
      (Just (is_bot, _, V inp r_bot, os_bot), g'') = match bot g'
      g''' = (rmdups $ is_top ++ is_bot, node_keep, V inp r_keep, os_bot) & g''
      -- TODO must pay more attention here in the future
      node_keep = min top bot
      r_keep = r_top || r_bot

  equate top bot
  modifyT $ const g'''


reduce2' b1 b2 = reduce2 (b1, b2)

reduce2 :: (BDD, BDD) -> KS ()
reduce2 (B 0, _) =  return ()
reduce2 (_, B 0) = return ()
reduce2 (B 1, _) = return ()
reduce2 (_, B 1) = return ()

reduce2 (B n1, B n2) = do
  g0 <- getG
  when (gelem n1 g0 && gelem n2 g0) $ do
    (z1, o1) <- flip getSons n1 <$> getT
    (z2, o2) <- flip getSons n2 <$> getT
    when (z1 == z2 && o1 == o2) $ do
      moveParents n1 n2

moveParents :: Node -> Node -> KS ()
moveParents n1 n2 = do
  g <- getT
  let (Just (is_n1, _, V inp r_n1, os_n1), g') = match n1 g
      (Just (is_n2, _, V _   r_n2, _    ), g'') = match n2 g'
      g''' = (rmdups $ is_n1 ++ is_n2, node_keep, V inp r_keep, os_n1) & g''
      node_keep = min n1 n2
      r_keep = r_n1 || r_n2
  modifyT $ const g'''
  when (r_n1 && r_n2) $ equate n1 n2 >> return ()

reduceGroup :: [BDD] -> KS ()
reduceGroup [] = return ()
reduceGroup [_] = return ()
reduceGroup (x:xs) = do
  mapM_ (reduce2' x) xs

reduceLayer :: [Node] -> KS ()
reduceLayer ls = do
  mapM_ (reduce1 . B) ls
  g <- getT
  mapM_ (reduceGroup . map B) $ groupWithSons g ls
  --mapM_ reduce2 [(B a, B b) | a <- ls, b <- ls, a < b]


getSize :: KS Int
getSize = (length . nodes) <$> getT


reduceAll :: KS ()
reduceAll = do
  g <- getT
    {-
  traceM $ "Layer: " ++ show (layers g) ++
    "\nGroups: " ++ show (map (groupWithSons g) (layers g)) ++
      "\nGraphviz: -> " ++ showBDD g ++
        "\nGraph: -> " ++ show g
        -}
  mapM_ reduceLayer $ layers g
  -- mapM_ bddPurge' (map B (nodes g))


-- Monadic functions show be bellow here

initialBDD_M :: Node -> KS BDD
initialBDD_M n =
  do modifyT $ initialBDD n
     return $ B n

negateBDDM b = do
  (bdd', g') <- negateBDD b <$> getT
  modify $ \s -> s { bdd = g' }
  return bdd'


