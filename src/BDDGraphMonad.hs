module BDDGraphMonad where

import BDDGraph
import Kuelmann97

import Control.Monad.Writer
import Data.Graph.Inductive
import Control.Monad.State
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.IntMap as M

data BDDStateD = S { graph :: T
                   , equals :: [(Node, Node)]
                   , ordering :: BDDOrdering }

type BDDState = StateT BDDStateD KS

logBDD :: String -> BDDState ()
logBDD c = do
  g <- getG
  tell ["// " ++ c ++ "\n" ++ showBDD g ++ "\n" ]


withGraph :: T -> (BDDState a) -> T
withGraph g op =
  fst $ flip evalState (S g [] compare) $ runWriterT $ (op >> getG)

runBDDState :: [Node] -> [Node] -> BDDOrdering -> (BDDState a)
            -> ((a, [String]), (T, [(Node, Node)]))
runBDDState is ns order op = (r, (graph s, equals s))
  where
    g = initialBDD' is $ reserveNodes ns startingG
    (r, s) = flip runState (S g [] order) $ runWriterT $ op


getG :: BDDState T
getG =  graph <$> get

modifyG :: (T -> T) -> BDDState ()
modifyG f = do
  s <- get
  put $ s { graph = f (graph s) }

equate :: Node -> Node -> BDDState ()
equate n1 n2 = do
  s <- get
  put $ s { equals = (n1, n2):(equals s) }

cashOut :: BDDState [(Node, Node)]
cashOut = do
  s <- get
  put $ s { equals = [] }
  return $ equals s


bddAndMany :: Maybe Node -> [BDD] -> BDDState BDD
bddAndMany n [] = error $ "cannot conjoin nothing"
bddAndMany n [b] = bddAnd n (B 1) b
bddAndMany n [a, b] = bddAnd n a b
bddAndMany n (a:os) = do
  r <- bddAndMany Nothing os
  bddAnd n a r


bddAndRepr :: Node -> BDD -> BDD ->  BDDState BDD
bddAndRepr n b1 b2 = do
  --g <- getG
  --tell ["// bddAndRepr - before " ++ show (n, b1, b2) ++ "\n" ++ showBDD g ++ "\n" ]
  bddAnd (Just n) b1 b2

getOrdering :: Node -> Node -> BDDState Ordering
getOrdering n1 n2 = do
  {-
  return $ n1 `compare` n2
  --traceM $ " get Ordering " ++ show (n1, n2)
  -}
  m <- ordering <$> get
  --let Just o1 = M.lookup n1 m
  --    Just o2 = M.lookup n2 m
  return $ n1 `m` n2

newParentM :: Maybe Node -> Node -> (Node, Node) -> BDDState BDD
newParentM repr orig (l, r) = do
  g <- getG
  let (bdd', g') = newParent repr orig (l,r) g
  modifyG $ const g'
  return bdd'

bddAnd :: Maybe Node -> BDD -> BDD -> BDDState BDD

bddAnd Nothing (B 0) _ =
  return $ B 0

bddAnd (Just x) (B 0) _ = do
  equate 0 x
  return $ B 0

bddAnd repr _ (B 0) = bddAnd repr (B 0) undefined

bddAnd repr (B 1) (B 1) = return $ B 1

bddAnd repr (B 1) (B b) = do
  (l, r) <- flip  getSons b <$> getG
  newParentM repr b (l, r)

bddAnd repr b (B 1) = bddAnd repr (B 1) b

bddAnd repr (B n1) (B n2) = do
  v_n1 <- inputNode n1 <$> getG
  v_n2 <- inputNode n2 <$> getG
  (z1, o1) <- flip getSons n1 <$> getG
  (z2, o2) <- flip getSons n2 <$> getG

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
  modifyG $ insEdges [(n, z, False), (n, o, True)]

reduce1 :: BDD -> BDDState ()
reduce1 (B 0) = return ()
reduce1 (B 1) = return ()
reduce1 (B n) = do
  (z, o) <- flip getSons n <$> getG
  when (z == o) $ do
    modifyG $ delEdges [(n,z), (n,o)]
    mergeNodes1 n z

mergeNodes1 :: Node -> Node -> BDDState()
mergeNodes1 top bot = do
  g <- getG
  let (Just (is_top, _, V _   r_top, _), g') = match top g
      (Just (is_bot, _, V inp r_bot, os_bot), g'') = match bot g'
      g''' = (rmdups $ is_top ++ is_bot, node_keep, V inp r_keep, os_bot) & g'' -- TODO must pay more attention here in the future
      node_keep = min top bot
      r_keep = r_top || r_bot

  when (r_top && r_bot) $ equate top bot
  modifyG $ const g'''


reduce2' b1 b2 = reduce2 (b1, b2)

reduce2 :: (BDD, BDD) -> BDDState ()
reduce2 (B 0, _) =  return ()
reduce2 (_, B 0) = return ()
reduce2 (B 1, _) = return ()
reduce2 (_, B 1) = return ()

reduce2 (B n1, B n2) = do
  g0 <- getG
  when (gelem n1 g0 && gelem n2 g0) $ do
    (z1, o1) <- flip getSons n1 <$> getG
    (z2, o2) <- flip getSons n2 <$> getG
    when (z1 == z2 && o1 == o2) $ do
      moveParents n1 n2
    --tell ["// reduce2 - before " ++ show (n1, n2) ++ "\n" ++ showBDD g0 ++ "\n" ]
    --tell ["// reduce2 - after " ++ show (n1, n2) ++ "\n" ++ showBDD g ++ "\n" ]

{-
-- | moveParents' removes the parents of the first
-- node n1 and adds it to n2.
moveParents' :: Node -> Node -> BDDState ()
moveParents' n1 n2 = do
  ps_n1 <- flip inn n1 <$> getG
  modifyG $ delEdges [(o, d) | (o, d, _) <- ps_n1]
  modifyG $ insEdges [(o, n2, v) | (o,_,v) <- ps_n1]
  bddPurge (B n1)
-}

moveParents :: Node -> Node -> BDDState ()
moveParents n1 n2 = do
  g <- getG
  let (Just (is_n1, _, V inp r_n1, os_n1), g') = match n1 g
      (Just (is_n2, _, V _   r_n2, _    ), g'') = match n2 g'
      g''' = (rmdups $ is_n1 ++ is_n2, min n1 n2, V inp (r_n1 || r_n2), os_n1) & g''

  when (r_n1 && r_n2) $ equate n1 n2
  modifyG $ const g'''

reduceGroup :: [BDD] -> BDDState ()
reduceGroup [] = return ()
reduceGroup [_] = return ()
reduceGroup (x:xs) = do
  mapM_ (reduce2' x) xs

reduceLayer :: [Node] -> BDDState ()
reduceLayer ls = do
  mapM_ (reduce1 . B) ls
  g <- getG
  mapM_ (reduceGroup . map B) $ groupWithSons g ls
  --mapM_ reduce2 [(B a, B b) | a <- ls, b <- ls, a < b]


getSize :: BDDState Int
getSize = (length . nodes) <$> getG


reduceAll :: BDDState ()
reduceAll = do
  g <- getG
    {-
  traceM $ "Layer: " ++ show (layers g) ++
    "\nGroups: " ++ show (map (groupWithSons g) (layers g)) ++
      "\nGraphviz: -> " ++ showBDD g ++
        "\nGraph: -> " ++ show g
        -}
  mapM_ reduceLayer $ layers g
  -- mapM_ bddPurge' (map B (nodes g))


-- Monadic functions show be bellow here

initialBDD_M :: Node -> BDDState BDD
initialBDD_M n =
  do modifyG $ initialBDD n
     return $ B n

negateBDDM b = do
  (bdd', g') <- negateBDD b <$> getG
  modifyG $ const g'
  return bdd'



