--module BDDGraph (BDD, initialBDD, negateBDD ) where
module BDDGraph where

import Debug.Trace
import Data.Graph.Inductive
import Data.Graph.Inductive.Query.DFS (reachable)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.List (intersperse)
import qualified Data.Set as S

instance Monoid BDD where
  mempty = bddOne
  mappend = bddAnd

data NV = I Node | Zero | One
  deriving (Eq, Show)

type G = Gr NV Bool
type Ctx = Context NV Bool
data BDD = BDD { getG :: G , getN :: Node }
  deriving Show

instance Ord NV where
  Zero `compare` Zero = EQ
  Zero `compare` _    = LT
  One  `compare` One  = EQ
  One  `compare` _    = LT
  I x  `compare` I y  = x `compare` y

type GST a = State G a


instance Ord BDD where
  b1@(BDD g1 n1) `compare` b2@(BDD g2 n2) =
    case val g1 n1 `compare` val g2 n2 of
      LT -> LT
      GT -> GT
      EQ -> case leftOf b1 `compare` leftOf b2 of
              LT -> LT
              GT -> GT
              EQ -> rightOf b1 `compare` rightOf b2

instance Eq BDD where
  b1 == b2 = (b1 `compare` b2) == EQ


leftOf :: BDD -> BDD
leftOf (BDD g n) =
  runGST g (do (l, _) <- bddSucM n
               return (BDD g l))

rightOf :: BDD -> BDD
rightOf (BDD g n) =
  runGST g (do (_, r) <- bddSucM n
               return (BDD g r))

runGST :: G -> GST a -> a
runGST g c = evalState c g


--runGST2 :: BDD -> BDD -> (Node -> Node -> GST a) -> a
--runGST2 (BDD g1 n1) (BDD g2 n2) c =
--  runGST g1 (do r <- bundle n2 g2
--                c n1 r)


adjustRange :: (Node, Node) -> Node -> Node
adjustRange (_, r) n
  | n == 0 = trace "\n----ZERO----\n" n
  | n == 1 = trace "\n----ONE----\n" n
  | otherwise =  trace "\n----OTHER----\n" $  n + r

adjustAdjRange :: (Node, Node) -> (Bool, Node) -> (Bool, Node)
adjustAdjRange r (b, n) = (b, adjustRange r n)

adjustNodeRange :: (Node, Node) -> G -> G
adjustNodeRange r g =
  gmap (\(is, n, v, os) ->
    ( map (adjustAdjRange r) is
    , adjustRange r n
    , v
    , map (adjustAdjRange r) os)) g

contexts :: G -> [Ctx]
contexts g = map (context g) (nodes g)


uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

merge :: G -> G -> Node -> (G, Node)
merge g1 g2 n = traceShow (g1, g2, g2', n) $
  (foldr (&) g1 (contexts g2'), n')
  where
    g2' = adjustNodeRange (nodeRange g1) g2
    --g2' = adjustNodeRange (nodeRange g1) subgraph (reachable n g2) g2
    n'  = adjustRange (nodeRange g1) n

bundle :: Node -> G -> GST Node
bundle n g2 = do
  g1 <- get
  let (g', n') = merge g1 g2 n
  put g'
  return n'

bddOne  = BDD (mkGraph [(1, One)] []) 1
bddZero = BDD (mkGraph [(0, Zero)] []) 0

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
  return $ fromMaybe (error $ "tried to compare inexistent nodes: " ++ show (g, n1, n2)) $
    compare <$> lab g n1 <*> lab g n2


newNode :: GST Node
newNode = head . newNodes 1 <$> get

bddSucM :: Node -> GST (Node, Node)
bddSucM n = do
  g <- get
  ss <- flip lsuc n <$> get
  let [(l, lv), (r, rv)] = uniq ss
        --trace ("ss -> " ++ show ss ++ "-> "++ prettify g) (uniq ss)
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
bddAndM n1 n2 = trace ("bddAndM " ++ show (n1, n2) ++ "\n") $ do
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
bddAnd (BDD g1 n1) (BDD g2 n2) = trace (":> bbdAnd " ++ show (n1, n2) ++"\n") $
  BDD g' t'
  where (g', t') = runGST g1 (do r <- bundle n2 g2
                                 t <- trace ("\n--->" ++ show (n1, n2, r)) (bddAndM n1 r)
                                 --t <- bddAndM n1 r
                                 g <- get
                                 return (g, t))


bddReduce :: BDD -> BDD
bddReduce = undefined


bddSize :: BDD -> Int
bddSize (BDD g _) = noNodes g