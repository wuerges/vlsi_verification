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
import qualified Data.IntMap as M


newtype BDD = B Int
  deriving (Eq, Ord, Show)

data V =  V { input :: Node
            , repr :: Bool }
  deriving Show

type T = Gr V Bool

labelN (n, v) = [("label", label)] ++ shape
  where label = show n ++ "," ++ show (input v)
        shape = if repr v then [("shape","rectangle")] else []

-- | Converts a graph to a GraphViz format
showBDD g = showDot $ do
  --fglToDot $ gmap (\(is, n, v, os) -> ([], n, (n, input v, repr v), [])) g
  forM_ (labNodes g) $ \(n, v) -> userNode (userNodeId n)  (labelN (n, v))

  forM_ (labEdges g) $
    \(o, d, t) -> edge (userNodeId o) (userNodeId d) (if t then [] else [("style","dotted")])
  mapM_ (same . map userNodeId) (layers g)

type Ctx = Context V Bool

data BDDStateD = S { graph :: T
                   , equals :: [(Node, Node)]
                   , ordering :: M.IntMap Int }

--type BDDState a = StateT (T, [(Node,Node)]) (Writer [String]) a
type BDDState = WriterT [String] (State (T, [(Node,Node)]))

logBDD :: String -> BDDState ()
logBDD c = do
  g <- getG
  tell ["// " ++ c ++ "\n" ++ showBDD g ++ "\n" ]


startingG = mkGraph [(0,V (-1) True), (1, V (-1) True)] [] :: T

reserveNodes :: [Node] -> BDDState ()
reserveNodes ns =
  modifyG $ insNodes $ zip ns (repeat (V (-1) True))

 {- For a StateT with a writer
--type BDDState a = StateT (T, [(Node,Node)]) (Writer [String]) a
  -}
runBDDState is ns op = flip runState (startingG, []) $ runWriterT  $ do
  reserveNodes ns
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

modifyG :: (T -> T) -> BDDState ()
modifyG f = do
  (g, m) <- get
  put (f g, m)

equate :: Node -> Node -> BDDState ()
equate n1 n2 = do
  (g, m) <- get
  put (g, (n1, n2):m)

cashOut :: BDDState [(Node, Node)]
cashOut = do
  (g, m) <- get
  put (g, [])
  return m

initialBDD :: Node -> BDDState BDD
initialBDD n = do
  g <- getG
  let (Just (_, _, v, _), g') = match n g
      g'' = ([], n, V n True, [(True, 1), (False, 0)]) & g
  modifyG $ const g''
  return $ B n
    {-
  unless (gelem n g) $ do
    modifyG $ insNode (n, V n True)
    modifyG $ insEdges [(n, 0, False), (n, 1, True)]
  return $ B n
  -}

-- | Exported function
bddOne = B 1
-- | Exported function
bddZero = B 0

inputNodeM :: Node -> BDDState Node
inputNodeM o = do
  Just v <- flip lab o <$> getG
  return $ input v

newNode :: T -> Node
newNode = head . newNodes 1

dupNode :: Maybe Node -> Node -> BDDState Node
dupNode repr orig  = do
  g <- getG
  v0 <- inputNodeM orig

  case repr of
    Nothing -> do
      let z = newNode g
      modifyG $ insNode (z, V v0 False)
      return z
    Just x -> do
      --let (_, g') = match x g
      --    g'' = ([],x, V v0 True, []) & g'
      --modifyG $ const g''
      modifyG $ insNode (x, V v0 True)
      return x

{-
  let (n_id, r) =
        case repr of
          Just x -> if (gelem x g) then error $ "Node ID was already in the graph: " ++ show x ++ "\n" ++ showBDD g
                                   else (x, True)
          Nothing -> (head $ newNodes 1 g, False)
  return n_id
  -}


getSonsG :: T -> Node -> (Node, Node)
getSonsG g n = case out g n of
                 [(_, l, False), (_, r, True)] -> (l, r)
                 [(_, r, True), (_, l, False)] -> (l, r)
                 x -> error ("// getSons: x was unexpected: " ++ show (n, x) ++ "\n" ++ showBDD g ++ "\n")


getSons :: Node -> BDDState (Node, Node)
getSons n = flip getSonsG n <$> getG

getL :: Node ->  BDDState Node
getL n = fst <$> getSons n
getR :: Node ->  BDDState Node
getR n = snd <$> getSons n

newParent :: Maybe Node -> Node -> (Node, Node) -> BDDState BDD
newParent repr orig (l, r) = do
  n' <- dupNode repr orig
  modifyG $ insEdges [(n', l, False), (n', r, True)]
  return $ B n'

-- | Exported function
negateBDD :: BDD -> BDDState BDD
negateBDD (B 0) = return $ B 1
negateBDD (B 1) = return $ B 0
negateBDD (B n) = do
  -- v <- inputNodeM n
  (l, r) <- getSons n
  B l' <- negateBDD $ B l
  B r' <- negateBDD $ B r
  newParent Nothing n (l', r')

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
  g <- getG
  let Just (V _ r) = lab g n
  unless r $ bddPurge (B n)


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

bddAnd :: Maybe Node -> BDD -> BDD -> BDDState BDD

bddAnd Nothing (B 0) _ =
  return $ B 0

bddAnd (Just x) (B 0) _ = do
  equate 0 x
  return $ B 0

bddAnd repr _ (B 0) = bddAnd repr (B 0) undefined

bddAnd repr (B 1) (B 1) = return $ B 1

bddAnd repr (B 1) (B b) = do
  (l, r) <- getSons b
  newParent repr b (l, r)

bddAnd repr b (B 1) = bddAnd repr (B 1) b


bddAnd repr (B n1) (B n2) = do
  v_n1 <- inputNodeM n1
  v_n2 <- inputNodeM n2
  (z1, o1) <- getSons n1
  (z2, o2) <- getSons n2

  case v_n1 `compare` v_n2 of
    GT -> do
      B z <- bddAnd Nothing (B z1) (B n2)
      B o <- bddAnd Nothing (B o1) (B n2)
      newParent repr n1 (z, o)
    LT -> do
      B z <- bddAnd Nothing (B z2) (B n1)
      B o <- bddAnd Nothing (B o2) (B n1)
      newParent repr n2 (z, o)
    EQ -> do
      B z <- bddAnd Nothing (B z1) (B z2)
      B o <- bddAnd Nothing (B o1) (B o2)
      newParent repr n1 (z, o)

setSons n z o =
  modifyG $ insEdges [(n, z, False), (n, o, True)]

reduce1 :: BDD -> BDDState ()
reduce1 (B 0) = return ()
reduce1 (B 1) = return ()
reduce1 (B n) = do
  (z, o) <- getSons n
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
    (z1, o1) <- getSons n1
    (z2, o2) <- getSons n2
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

rmdups = map head . group . sort

moveParents :: Node -> Node -> BDDState ()
moveParents n1 n2 = do
  g <- getG
  let (Just (is_n1, _, V inp r_n1, os_n1), g') = match n1 g
      (Just (is_n2, _, V _   r_n2, _    ), g'') = match n2 g'
      g''' = (rmdups $ is_n1 ++ is_n2, min n1 n2, V inp (r_n1 || r_n2), os_n1) & g''

  when (r_n1 && r_n2) $ equate n1 n2
  modifyG $ const g'''

sortAndGroupBy p = groupBy (equating p) . sortBy (comparing p)
  where equating p x y = (p x) == (p y)

groupWithSons g = map (map fst) . sortAndGroupBy snd . map (\n -> (n, getSonsG g n)) . filter (flip gelem g)

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


ginput :: (Node, V) -> Node
ginput (_,v) = input v

layers :: T -> [[Node]]
layers = map (map fst) . sortAndGroupBy ginput . filter (\x -> ginput x > 0) . labNodes
  {-
  where ns :: [(Node, V)]
        ns = labNodes gr
        ns' = filter t ns
        f a b = ginput a `compare` ginput b
        g a b = ginput a == ginput b
        t a = ginput a > 0
        -}


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

