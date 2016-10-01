module BDDGraph (NV) where

import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS (reachable)
import Control.Monad.State
import Data.Maybe (fromMaybe)

instance Monoid BDD where
  mempty = bddOne
  mappend = bddAnd

data NV = I Node | Zero | One
  deriving (Eq, Ord, Show)

type G = Gr NV Bool
type Ctx = Context NV Bool
data BDD = BDD G Node

type GST a = State G a

runGST :: G -> GST a -> a
runGST g c = evalState c g

adjustRange :: (Node, Node) -> Node -> Node
adjustRange (_, r) n = n + r

adjustNodeRange :: (Node, Node) -> G -> G
adjustNodeRange r =
  gmap (\(ctx@(is, n, v, os)) ->
      case v of
        (I x) -> (is, adjustRange r n, (I x), os)
        _     -> ctx)

contexts :: G -> [Ctx]
contexts g = map (context g) (nodes g)

merge :: G -> G -> G
merge g1 g2 = foldr (&) g1 (contexts g2)

bundle :: Node -> G -> GST Node
bundle n g2 = do
  g1 <- get
  let g2' = adjustNodeRange (nodeRange g1) $ subgraph (reachable n g2) g2
  put (merge g1 g2')
  return $ adjustRange (nodeRange g1) n



bddOne  = BDD (mkGraph [(1, One)] []) (-1)
bddZero = BDD (mkGraph [(0, Zero)] []) (-1)

initialBDD :: Int -> BDD
initialBDD v = BDD g nv
    where nv = v + 2
          g = mkGraph [(0, Zero), (1, One), (nv, I v)]
                      [(nv, 0, False), (nv, 1, True)]


negateBDD :: BDD -> BDD
negateBDD (BDD g v) = BDD (nmap negateNV g) v
  where
    negateNV Zero = One
    negateNV One = Zero
    negateNV (I x) = (I x)

larger :: Node -> Node -> GST Ordering
larger n1 n2 = do
  g <- get
  return $ fromMaybe (error "tried to compar inexistent nodes") $
    compare <$> lab g n1 <*> lab g n2


newNode :: GST Node
newNode = head . newNodes 1 <$> get

bddSucM :: Node -> GST (Node, Node)
bddSucM n = do
  ss@[(l, lv), (r, rv)] <- flip lsuc n <$> get
  case (lv, rv) of
    (False, True) -> return (l, r)
    (True, False) -> return (r, l)
    _ -> error $ "BDD sucs not ok: " ++ show ss



val :: G -> Node -> NV
val g n = fromMaybe (error "Could not find node in BDD") $
  lab g n


addParent :: (Node, Node) -> Node -> GST Node
addParent (l, r) p = do
  t <- newNode
  g <- get
  put $ ([], t, val g p, [(False, l), (True, r)]) & g
  return t


bddAndM :: Node -> Node -> GST Node
bddAndM 0 _ = return 0
bddAndM 1 n = return n
bddAndM n 0 = return 0
bddAndM n 1 = return n
bddAndM n1 n2 = do
  ord <- larger n1 n2
  case ord of
    EQ -> do
      (l1, r1) <- bddSucM n1
      (l2, r2) <- bddSucM n2
      l <-bddAndM l1 l2
      r <-bddAndM r1 r2
      addParent (l, r) n1
    LT -> do
      (l1, r1) <- bddSucM n1
      l <-bddAndM l1 n2
      r <-bddAndM r1 n2
      addParent (l, r) n2
    GT -> do
      (l2, r2) <- bddSucM n2
      l <-bddAndM n1 l2
      r <-bddAndM n1 r2
      addParent (l, r) n1

bddAnd :: BDD -> BDD -> BDD
bddAnd (BDD g1 n1) (BDD g2 n2) = BDD g' t'
  where (g', t') = runGST g1 (do r <- bundle n2 g2
                                 t <- bddAndM n1 r
                                 g <- get
                                 return (g, t))


bddReduce :: BDD -> BDD
bddReduce = undefined


bddSize :: BDD -> Int
bddSize (BDD g _) = noNodes g
