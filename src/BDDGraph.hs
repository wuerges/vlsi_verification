--module BDDGraph (BDD, initialBDD, negateBDD ) where
module BDDGraph where

import Text.Dot
import Debug.Trace
import Control.Monad.Writer
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Control.Monad.State
import Data.Ord
import Data.List
import Data.Maybe

newtype BDD = B Int
  deriving (Eq, Ord, Show)

data V =  V { input :: Node
            , repr :: Maybe Node }
  deriving Show

type T = Gr V Bool

labelN (n, v) = [("label", label)] ++ maybe [] (const [("shape","rectangle")]) (repr v)
  where label = show n ++ "," ++ show (input v) ++ "," ++ r
        r = maybe "" show (repr v)

-- | Converts a graph to a GraphViz format
showBDD :: T -> String
showBDD g = showDot $ do
  --fglToDot $ gmap (\(is, n, v, os) -> ([], n, (n, input v, repr v), [])) g
  forM_ (labNodes g) $ \(n, v) -> userNode (userNodeId n)  (labelN (n, v))

  forM_ (labEdges g) $
    \(o, d, t) -> edge (userNodeId o) (userNodeId d) (if t then [] else [("style","dotted")])
  mapM_ (same . map userNodeId) (layers g)


type Ctx = Context V Bool

--type BDDState a = StateT (T, [(Node,Node)]) (Writer [String]) a
type BDDState = WriterT [String] (State (T, [(Node,Node)]))

startingG = mkGraph [(0,V (-1) (Just 0)), (1, V (-1) (Just 1))] [] :: T

 {- For a StateT with a writer
--type BDDState a = StateT (T, [(Node,Node)]) (Writer [String]) a
  -}
runBDDState is op = flip runState (startingG, []) $ runWriterT  $ do
  mapM initialBDD is
  op

 {- For a StateT with a writer
--type BDDState a = StateT (T, [(Node,Node)]) (Writer [String]) a
runBDDState is op = runWriter $ flip runStateT (startingG, []) $ do
  mapM initialBDD is
  op
  -}

 {- -- For a simple state monad
type BDDState a = State (T, [(Node,Node)]) a
runBDDState is op = flip runState (startingG, []) $ do
  mapM initialBDD is
  op
-}
getG :: BDDState T
getG = fst <$> get

owner :: T -> Node -> Maybe Node
owner g n = r
  where Just (V _ r) = lab g n

modifyG :: (T -> T) -> BDDState ()
modifyG f = do
  (g, m) <- get
  put (f g, m)

equate :: Maybe Node -> Maybe Node -> BDDState ()
equate n1 n2 = do
  (g, m) <- get
  case (n1, n2) of
    (Just j1, Just j2) -> put (g, (j1, j2):m)
    _ -> return ()

cashOut :: BDDState [(Node, Node)]
cashOut = do
  (g, m) <- get
  put (g, [])
  return m

initialBDD :: Node -> BDDState BDD
initialBDD n = do
  g <- getG
  unless (gelem n g) $ do
    modifyG $ insNode (n, V n (Just n))
    modifyG $ insEdges [(n, 0, False), (n, 1, True)]
  return $ B n

-- | Exported function
bddOne = B 1
-- | Exported function
bddZero = B 0

bval :: Node -> BDDState Node
bval o = do
  Just v <- flip lab o <$> getG
  return $ input v

dupNode :: V -> BDDState Node
dupNode (V v r) = do
  [n] <- newNodes 1 <$> getG
  modifyG $ insNode (n, V v r)
  return n

getSons :: Node -> BDDState (Node, Node)
getSons n = do
  es <- flip out n <$> getG
  g <- getG
  case es of
    [(_, l, False), (_, r, True)] -> return (l, r)
    [(_, r, True), (_, l, False)] -> return (l, r)
    x -> do --tell [ "// x was unexpected: " ++ show x ++ "\n" ++ showBDD g ++ "\n"]
            return (0, 0)

getL :: Node ->  BDDState Node
getL n = fst <$> getSons n
getR :: Node ->  BDDState Node
getR n = snd <$> getSons n

newParent :: V -> (Node, Node) -> BDDState BDD
newParent n (l, r) = do
  n' <- dupNode n
  modifyG $ insEdges [(n', l, False), (n', r, True)]
  return $ B n'

-- | Exported function
negateBDD :: BDD -> BDDState BDD
negateBDD (B 0) = return $ B 1
negateBDD (B 1) = return $ B 0
negateBDD (B n) = do
  v <- bval n
  (l, r) <- getSons n
  B l' <- negateBDD $ B l
  B r' <- negateBDD $ B r
  newParent (V v Nothing) (l', r')

bddPurge :: BDD -> BDDState ()
bddPurge (B 0) = return ()
bddPurge (B 1) = return ()
bddPurge (B n) = do
  ps <- flip inn n <$> getG
  when (null ps) $ do
    (l, r) <- getSons n
    modifyG $ delNode n
    bddPurge' (B l)
    bddPurge' (B r)


bddPurge' :: BDD -> BDDState ()
bddPurge' (B n) = do
  r <- reprM n
  unless (isJust r) $ bddPurge (B n)


bddAndMany :: Maybe Node -> [BDD] -> BDDState BDD
bddAndMany n [] = error $ "cannot conjoin nothing"
bddAndMany n [b] = bddAnd n (B 1) b
bddAndMany n [a, b] = bddAnd n a b
bddAndMany n (a:os) = do
  r <- bddAndMany Nothing os
  bddAnd n a r


bddAndRepr :: Node -> BDD -> BDD ->  BDDState BDD
bddAndRepr n b1 b2 = do
  g <- getG
  --tell ["// bddAndRepr - before " ++ show (n, b1, b2) ++ "\n" ++ showBDD g ++ "\n" ]
  bddAnd (Just n) b1 b2

bddAnd :: Maybe Node -> BDD -> BDD -> BDDState BDD

bddAnd Nothing (B 0) _ =
  return $ B 0

bddAnd (Just x) (B 0) _ = do
  equate (Just 0) (Just x)
  return $ B 0

bddAnd repr _ (B 0) = bddAnd repr (B 0) undefined

bddAnd repr (B 1) (B 1) = return $ B 1

bddAnd repr (B 1) (B b) = do
  v <- bval b
  (l, r) <- getSons b
  newParent (V v repr) (l, r)

bddAnd repr b (B 1) = bddAnd repr (B 1) b


bddAnd repr (B n1) (B n2) = do
  v_n1 <- bval n1
  v_n2 <- bval n2
  (z1, o1) <- getSons n1
  (z2, o2) <- getSons n2

  case v_n1 `compare` v_n2 of
    GT -> do
      B z <- bddAnd Nothing (B z1) (B n2)
      B o <- bddAnd Nothing (B o1) (B n2)
      newParent (V v_n1 repr) (z, o)
    LT -> do
      B z <- bddAnd Nothing (B z2) (B n1)
      B o <- bddAnd Nothing (B o2) (B n1)
      newParent (V v_n2 repr) (z, o)
    EQ -> do
      B z <- bddAnd Nothing (B z1) (B z2)
      B o <- bddAnd Nothing (B o1) (B o2)
      newParent (V v_n1 repr) (z, o)

setSons n z o =
  modifyG $ insEdges [(n, z, False), (n, o, True)]

reduce1 :: BDD -> BDDState ()
reduce1 (B 0) = return ()
reduce1 (B 1) = return ()
reduce1 (B n) = do
  (z, o) <- getSons n
  when (z == o) $ do
    modifyG $ delEdges [(n,z), (n,o)]
    g <- getG
    equate (owner g z) (owner g n)
    (z', o') <- getSons z
    setSons n z' o'

reduce2 :: (BDD, BDD) -> BDDState ()
reduce2 (B 0, _) =  return ()
reduce2 (_, B 0) = return ()
reduce2 (B 1, _) = return ()
reduce2 (_, B 1) = return ()

reduce2 (B n1, B n2) = do
  (z1, o1) <- getSons n1
  (z2, o2) <- getSons n2
  when (z1 == z2 && o1 == o2) $ do
    g0 <- getG
    moveParents n1 n2
    g <- getG
    --tell ["// reduce2 - before " ++ show (n1, n2) ++ "\n" ++ showBDD g0 ++ "\n" ]
    --tell ["// reduce2 - after " ++ show (n1, n2) ++ "\n" ++ showBDD g ++ "\n" ]
    equate (owner g n1) (owner g n2)


moveParents' :: Node -> Node -> BDDState ()
moveParents' n1 n2 = do
  ps_n1 <- flip inn n1 <$> getG
  modifyG $ delEdges [(o, d) | (o, d, _) <- ps_n1]
  modifyG $ insEdges [(o, n2, v) | (o,_,v) <- ps_n1]


reprM :: Node -> BDDState (Maybe Node)
reprM n = do
  g <- getG
  let Just v = lab g n
  return $ repr v


moveParents :: Node -> Node -> BDDState ()
moveParents n1 n2 = do
  r <- reprM n1
  maybe (moveParents' n1 n2) (\_ -> moveParents' n2 n1) r

reduceLayer :: [Node] -> BDDState ()
reduceLayer ls = do
  mapM_ (reduce1 . B) ls
  mapM_ reduce2 [(B a, B b) | a <- ls, b <- ls, a < b]



ginput :: (Node, V) -> Node
ginput (_,v) = input v

layers :: T -> [[Node]]
layers g = map (map fst) $ groupBy (\a b -> ginput a == ginput b) ns
  where ns = sortBy f $ labNodes g
        f a b = ginput a `compare` ginput b


reduceAll :: BDDState ()
reduceAll = do
  g <- getG
  mapM_ reduceLayer $ layers g
  --mapM_ bddPurge' (map B (nodes g))

